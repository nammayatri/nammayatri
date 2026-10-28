{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverCityMigration
  ( migrateDriverOperatingCity,
  )
where

import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.DocumentVerificationConfig as DTO
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common (MonadFlow)
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
  ( CacheFlow,
    Hours,
    fromMaybeM,
    getCurrentTime,
  )
import qualified Lib.Yudhishthira.Tools.Utils as YTUtils
import qualified Lib.Yudhishthira.Types as LYT
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverInformationExtra as QDriverInfo
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.DriverProfileQuestions as QDriverProfileQuestions
import qualified Storage.Queries.DriverReferral as QDriverReferral
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QReg
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleExtra as QVehicleExtra
import Tools.Error
import Tools.Notifications (notifyDriver)

-- | Moves a driver to targetOpCityId: auth-layer sync, notification, billing tag,
-- doc-gap notifications and operational-table sync. Shared by the ride-end
-- migration (kafka-consumers) and the driver-initiated operating city update API.
--
-- The entire migration is gated by ONE Redis lock keyed on the driver, so concurrent
-- callers for the same driver don't run it twice.
migrateDriverOperatingCity ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
migrateDriverOperatingCity driverPerson merchantId targetOpCityId =
  Redis.whenWithLockRedis (driverCityMigrationLockKey driverPerson.id) driverCityMigrationLockTtl $ do
    -- --- auth layer: mirrors Dashboard.Driver.postDriverChangeOperatingCity's manual flow ---
    QPerson.updateMerchantOperatingCityId driverPerson.id targetOpCityId
    QReg.updateMerchantOperatingCityId targetOpCityId.getId driverPerson.id.getId merchantId.getId
    DReg.cleanCachedTokens driverPerson.id
    targetOpCity <- CQMOC.findById targetOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist targetOpCityId.getId)
    notifyDriver
      targetOpCityId
      FCM.NEW_MESSAGE
      "Operating City Updated"
      ("Your operating city has been updated to " <> show targetOpCity.city <> ".")
      driverPerson
      driverPerson.deviceToken

    -- --- billing: tag only, never auto-switch (per-city pricing differs) ---
    mbDriverPlan <- QDPlan.findByPrimaryKey driverPerson.id
    let mbBillingTag =
          mbDriverPlan >>= \p ->
            if p.merchantOpCityId /= targetOpCityId
              then
                Just
                  ( LYT.TagNameValue "SubscriptionMigrationPending",
                    "Your operating city has changed. Please visit the plan page to select a plan for your current city."
                  )
              else Nothing

    -- --- documents: diff required-vs-held, dedup-tag, notify. Uses the ForEnabling
    -- notion (fromMaybe isMandatory isMandatoryForEnabling), not plain isMandatory --
    -- isMandatory alone misses "enabling-only" docs (isMandatory=false,
    -- isMandatoryForEnabling=true, e.g. MSDS/Medical), so a driver needing one of
    -- those to go online in the new city would otherwise never be told. ---
    mbVehicle <- QVehicle.findById driverPerson.id
    docTags <- case mbVehicle >>= (.category) of
      Nothing -> pure []
      Just vehicleCategory -> do
        allConfigs <- CQDVC.findAllByMerchantOpCityId targetOpCityId Nothing
        let requiredDocTypes =
              [ c.documentType
                | c <- allConfigs,
                  fromMaybe c.isMandatory c.isMandatoryForEnabling,
                  not c.isDisabled,
                  c.vehicleCategory == vehicleCategory
              ]
        missingDocTypes <- filterM (fmap not . isDocumentHeld driverPerson.id merchantId) requiredDocTypes
        pure
          [ ( LYT.TagNameValue ("MissingDocumentForCity_" <> show docType <> "#" <> targetOpCityId.getId),
              "Please upload " <> show docType <> " to continue operating in your current city."
            )
            | docType <- missingDocTypes
          ]

    applyNewTags driverPerson targetOpCityId (maybeToList mbBillingTag <> docTags)

    -- --- operational tables: sync directly, no gating. Document tables are
    -- intentionally excluded -- nothing reads their city column. ---
    QDriverInfo.updateMerchantIdAndCityIdByDriverId driverPerson.id merchantId targetOpCityId
    QVehicleExtra.updateMerchantIdAndCityIdByDriverId driverPerson.id merchantId (Just targetOpCityId.getId)
    QDailyStats.updateMerchantIdAndCityIdByDriverId (Just merchantId) (Just targetOpCityId) driverPerson.id
    QDriverReferral.updateMerchantIdAndCityIdByDriverId (Just merchantId) (Just targetOpCityId) driverPerson.id
    QDriverProfileQuestions.updateMerchantOperatingCityIdByDriverId targetOpCityId driverPerson.id
  where
    isDocumentHeld :: (CacheFlow m r, Esq.EsqDBFlow m r, MonadFlow m) => Id DP.Person -> Id DM.Merchant -> DTO.DocumentType -> m Bool
    isDocumentHeld personId merchantId' docType = do
      images <- QImage.findImagesByPersonAndType Nothing Nothing merchantId' personId docType
      pure $ any (\img -> img.verificationStatus == Just Documents.VALID) images

    -- Applies every candidate tag against ONE accumulating view of the driver's tag
    -- list, so a tag added earlier in this run is visible to the dedup check for a
    -- later one in the same run. existingTags is expiry-filtered up front -- relying
    -- on the read-side fromTType' filter alone isn't safe here, since EsqDBFlow reads
    -- can be served from a Redis-cached Person snapshot taken before a tag's embedded
    -- expiry passed, which would otherwise keep suppressing a fresh notification for
    -- an already-expired-but-still-unresolved gap. The final write-comparison is
    -- against this same filtered baseline, so a run that only prunes expired entries
    -- (no new tag needed) still persists that cleanup. Sends exactly one notification
    -- per genuinely new tag, targeted at the city the driver is migrating TO (not
    -- person.merchantOperatingCityId, which is stale here -- driverPerson was loaded
    -- before the city-update write above and is never refreshed).
    applyNewTags ::
      ( CacheFlow m r,
        Esq.EsqDBFlow m r,
        MonadFlow m,
        Redis.HedisFlow m r,
        Redis.HedisLTSFlowEnv r
      ) =>
      DP.Person ->
      Id DMOC.MerchantOperatingCity ->
      [(LYT.TagNameValue, Text)] ->
      m ()
    applyNewTags person opCityId candidateTags = do
      now <- getCurrentTime
      let existingTags = YTUtils.filterExpiredTags' now (fromMaybe [] person.driverTag)
      finalTags <- foldM (addTagIfAbsent person opCityId now) existingTags candidateTags
      when (YTUtils.showRawTags finalTags /= YTUtils.showRawTags existingTags) $
        QPerson.updateDriverTag (Just finalTags) person.id

    addTagIfAbsent ::
      ( CacheFlow m r,
        Esq.EsqDBFlow m r,
        MonadFlow m,
        Redis.HedisFlow m r,
        Redis.HedisLTSFlowEnv r
      ) =>
      DP.Person ->
      Id DMOC.MerchantOperatingCity ->
      UTCTime ->
      [LYT.TagNameValueExpiry] ->
      (LYT.TagNameValue, Text) ->
      m [LYT.TagNameValueExpiry]
    addTagIfAbsent person opCityId now acc (tag, body) =
      if YTUtils.elemTagNameValue tag acc
        then pure acc
        else do
          let tagWithExpiry = YTUtils.addTagExpiry tag (Just tagExpiryHours) now
          notifyDriver
            opCityId
            FCM.NEW_MESSAGE
            "City Update"
            body
            person
            person.deviceToken
          pure $ YTUtils.replaceTagNameValue (Just acc) tagWithExpiry

    tagExpiryHours :: Hours
    tagExpiryHours = 336 -- 14 days

driverCityMigrationLockKey :: Id DP.Person -> Text
driverCityMigrationLockKey driverId = "DriverCityMigration:Lock:" <> driverId.getId

driverCityMigrationLockTtl :: Int
driverCityMigrationLockTtl = 86400 -- whenWithLockRedis releases the lock as soon as the
-- migration finishes, so this only bounds a lock left behind by a crashed run. It is
-- NOT a debounce: a later migration for the same driver runs normally.
