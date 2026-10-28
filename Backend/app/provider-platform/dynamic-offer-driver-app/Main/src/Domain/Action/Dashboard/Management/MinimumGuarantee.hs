{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Admin side of Minimum Guarantee (MG) campaigns: create and edit a DRAFT, invite drivers, publish,
-- pause, cancel, extend, and read a day's enrollments. Publishing starts the campaign's settlement
-- job chain and notifies the invited drivers.
module Domain.Action.Dashboard.Management.MinimumGuarantee
  ( getMinimumGuaranteeList,
    postMinimumGuaranteeCreate,
    postMinimumGuaranteeUpdate,
    postMinimumGuaranteeInvite,
    postMinimumGuaranteeStatus,
    postMinimumGuaranteeExtend,
    getMinimumGuaranteeEnrollments,
  )
where

import qualified API.Types.ProviderPlatform.Management.MinimumGuarantee as Common
import qualified Control.Monad.Catch as C
import qualified Dashboard.Common as DCommon
import Data.List (nub, sortOn)
import qualified Data.Text as T
import Data.Time (Day)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MgCampaign as DMgC
import qualified Domain.Types.MgCampaignRule as DMgR
import qualified Domain.Types.MgCampaignWindow as DMgW
import qualified Domain.Types.MgEligibility as DMgEl
import qualified Domain.Types.MgEnrollment as DMgE
import qualified Domain.Types.Person as DP
import Environment
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..), MinimumGuaranteeSettlementJobData (..))
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.MinimumGuarantee.Context (setMgCampaignPaused)
import SharedLogic.MinimumGuarantee.Enrollment (cancelUpcomingEnrollments)
import SharedLogic.MinimumGuarantee.Rules (localDayOf, pageBounds)
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.MgCampaign as QMgC
import qualified Storage.Queries.MgCampaignExtra as QMgCx
import qualified Storage.Queries.MgCampaignRule as QMgR
import qualified Storage.Queries.MgCampaignWindow as QMgW
import qualified Storage.Queries.MgEligibility as QMgEl
import qualified Storage.Queries.MgEnrollment as QMgE
import qualified Storage.Queries.Person as QP
import qualified Tools.Notifications as Notify

resolveCity :: ShortId DM.Merchant -> Context.City -> Flow (DM.Merchant, DMOC.MerchantOperatingCity)
resolveCity merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  pure (merchant, merchantOpCity)

findScopedCampaign :: DMOC.MerchantOperatingCity -> Id DMgC.MgCampaign -> Flow DMgC.MgCampaign
findScopedCampaign merchantOpCity campaignId = do
  campaign <- QMgC.findByPrimaryKey campaignId >>= fromMaybeM (InvalidRequest $ "Campaign not found: " <> campaignId.getId)
  unless (campaign.merchantOperatingCityId == merchantOpCity.id) $
    throwError (InvalidRequest "Campaign belongs to a different operating city")
  pure campaign

-- | The city-local calendar day right now.
localToday :: DMOC.MerchantOperatingCity -> Flow Day
localToday merchantOpCity = do
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  now <- getCurrentTime
  pure $ localDayOf (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) now

--------------------------------------------------------------------------------
-- validation
--------------------------------------------------------------------------------

invalid :: Text -> Flow a
invalid = throwError . InvalidRequest

validateCampaignReq :: Common.MgCampaignReq -> Flow ()
validateCampaignReq req = do
  when (T.null (T.strip req.name)) $ invalid "name must not be empty"
  when (req.endDate < req.startDate) $ invalid "endDate must not be before startDate"
  when (req.guaranteeAmount <= 0) $ invalid "guaranteeAmount must be positive"
  when (any (< 0) [req.optInCutoffMinutes, req.settlementGraceMinutes]) $
    invalid "optInCutoffMinutes and settlementGraceMinutes must not be negative"
  when (maybe False (<= 0) req.maxBudget) $ invalid "maxBudget must be positive"
  when (null req.vehicleCategories) $ invalid "at least one vehicle category is required"
  validateWindows req.windows
  validateRules req.rules

-- | Windows are time-of-day slots inside one day and must not overlap.
validateWindows :: [Common.MgWindow] -> Flow ()
validateWindows windows = do
  when (null windows) $ invalid "at least one window is required"
  forM_ windows $ \w ->
    when (w.endTime <= w.startTime) $ invalid "each window must end after it starts, within one day"
  let sorted = sortOn (.startTime) windows
  when (or (zipWith (\a b -> b.startTime < a.endTime) sorted (drop 1 sorted))) $
    invalid "windows must not overlap"

validateRules :: [Common.MgRule] -> Flow ()
validateRules rules = do
  let metrics = map (.metric) rules
  when (length (nub metrics) /= length metrics) $ invalid "each metric may have at most one rule"
  forM_ rules $ \r -> validateSlabs r.slabs

-- | A slab runs until the next slab starts and the last is open-ended, so the set must start at 0,
-- have strictly increasing starts, and pay 0 to 100 percent.
validateSlabs :: [DMgR.MgSlab] -> Flow ()
validateSlabs slabs = do
  let sorted = sortOn (.fromValue) slabs
  case sorted of
    [] -> invalid "a rule needs at least one slab"
    (firstSlab : _) -> when (firstSlab.fromValue /= 0) $ invalid "the first slab must start at 0"
  forM_ sorted $ \s ->
    unless (s.percent >= 0 && s.percent <= 100) $ invalid "slab percent must be between 0 and 100"
  forM_ (zip sorted (drop 1 sorted)) $ \(a, b) ->
    unless (a.fromValue < b.fromValue) $ invalid "slabs must start at different values"

--------------------------------------------------------------------------------
-- building and reading campaigns
--------------------------------------------------------------------------------

buildCampaign ::
  Id DMgC.MgCampaign ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  UTCTime ->
  DMgC.MgCampaignStatus ->
  UTCTime ->
  Common.MgCampaignReq ->
  DMgC.MgCampaign
buildCampaign campaignId merchantId merchantOperatingCityId createdBy createdAt status now req =
  DMgC.MgCampaign
    { id = campaignId,
      merchantId,
      merchantOperatingCityId,
      name = req.name,
      description = req.description,
      vehicleCategories = req.vehicleCategories,
      startDate = req.startDate,
      endDate = req.endDate,
      frequency = DMgC.DAILY,
      guaranteeAmount = req.guaranteeAmount,
      currency = req.currency,
      forceAssign = req.forceAssign,
      optInCutoffMinutes = req.optInCutoffMinutes,
      settlementGraceMinutes = req.settlementGraceMinutes,
      onFleetChange = req.onFleetChange,
      maxBudget = req.maxBudget,
      status,
      createdBy,
      createdAt,
      updatedAt = now
    }

saveWindowsAndRules :: Id DMgC.MgCampaign -> UTCTime -> Common.MgCampaignReq -> Flow ()
saveWindowsAndRules campaignId now req = do
  windows <- forM req.windows $ \w -> do
    windowId <- generateGUID
    pure DMgW.MgCampaignWindow {id = windowId, campaignId, startTime = w.startTime, endTime = w.endTime, createdAt = now, updatedAt = now}
  QMgW.createMany windows
  rules <- forM req.rules $ \r -> do
    ruleId <- generateGUID
    pure DMgR.MgCampaignRule {id = ruleId, campaignId, metric = r.metric, params = r.params, slabs = r.slabs, createdAt = now, updatedAt = now}
  QMgR.createMany rules

toCampaignInfo :: DMgC.MgCampaign -> Flow Common.MgCampaignInfo
toCampaignInfo campaign = do
  windows <- QMgW.findAllByCampaignId campaign.id
  rules <- QMgR.findAllByCampaignId campaign.id
  pure
    Common.MgCampaignInfo
      { campaignId = cast campaign.id,
        name = campaign.name,
        description = campaign.description,
        vehicleCategories = campaign.vehicleCategories,
        startDate = campaign.startDate,
        endDate = campaign.endDate,
        frequency = campaign.frequency,
        guaranteeAmount = campaign.guaranteeAmount,
        currency = campaign.currency,
        forceAssign = campaign.forceAssign,
        optInCutoffMinutes = campaign.optInCutoffMinutes,
        settlementGraceMinutes = campaign.settlementGraceMinutes,
        onFleetChange = campaign.onFleetChange,
        maxBudget = campaign.maxBudget,
        status = campaign.status,
        createdBy = campaign.createdBy,
        createdAt = campaign.createdAt,
        windows = map (\w -> Common.MgWindow {startTime = w.startTime, endTime = w.endTime}) (sortOn (.startTime) windows),
        rules = map (\r -> Common.MgRule {metric = r.metric, params = r.params, slabs = r.slabs}) rules
      }

--------------------------------------------------------------------------------
-- handlers
--------------------------------------------------------------------------------

getMinimumGuaranteeList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Flow Common.MgCampaignListRes
getMinimumGuaranteeList merchantShortId opCity mbLimit mbOffset = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  let (limit, offset) = pageBounds mbLimit mbOffset
  campaigns <- QMgCx.findAllByMerchantOperatingCityIdPaged merchantOpCity.id limit offset
  infos <- mapM toCampaignInfo campaigns
  pure Common.MgCampaignListRes {campaigns = infos}

-- | `requestorId` is the authenticated dashboard user, supplied by the generated wiring, so a client
-- cannot name someone else as the creator.
postMinimumGuaranteeCreate :: ShortId DM.Merchant -> Context.City -> Text -> Common.MgCampaignReq -> Flow Common.MgCampaignRes
postMinimumGuaranteeCreate merchantShortId opCity requestorId req = do
  (merchant, merchantOpCity) <- resolveCity merchantShortId opCity
  validateCampaignReq req
  campaignId <- generateGUID
  now <- getCurrentTime
  QMgC.create (buildCampaign campaignId merchant.id merchantOpCity.id requestorId now DMgC.DRAFT now req)
  saveWindowsAndRules campaignId now req
  pure Common.MgCampaignRes {campaignId = cast campaignId}

postMinimumGuaranteeUpdate :: ShortId DM.Merchant -> Context.City -> Id DCommon.MgCampaign -> Common.MgCampaignReq -> Flow APISuccess
postMinimumGuaranteeUpdate merchantShortId opCity reqCampaignId req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  campaign <- findScopedCampaign merchantOpCity (cast reqCampaignId)
  unless (campaign.status == DMgC.DRAFT) $
    throwError (InvalidRequest "Only a DRAFT campaign can be edited; after publishing only the end date and the budget can change")
  validateCampaignReq req
  now <- getCurrentTime
  QMgC.updateByPrimaryKey (buildCampaign campaign.id campaign.merchantId campaign.merchantOperatingCityId campaign.createdBy campaign.createdAt DMgC.DRAFT now req)
  QMgW.deleteByCampaignId campaign.id
  QMgR.deleteByCampaignId campaign.id
  saveWindowsAndRules campaign.id now req
  pure Success

postMinimumGuaranteeInvite :: ShortId DM.Merchant -> Context.City -> Id DCommon.MgCampaign -> Common.MgInviteReq -> Flow Common.MgInviteRes
postMinimumGuaranteeInvite merchantShortId opCity reqCampaignId req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  campaign <- findScopedCampaign merchantOpCity (cast reqCampaignId)
  when (campaign.status `elem` [DMgC.CLOSED, DMgC.CANCELLED]) $
    throwError (InvalidRequest "Cannot invite drivers to a closed or cancelled campaign")
  now <- getCurrentTime
  invited <- forM req.invitees $ \invitee -> do
    let driverId = Id invitee.driverId :: Id DP.Person
    mbDriver <- QP.findById driverId
    mbExisting <- QMgEl.findByCampaignIdAndDriverId campaign.id driverId
    case (mbDriver, mbExisting) of
      (Just driver, Nothing)
        | driver.merchantOperatingCityId == merchantOpCity.id -> do
          eligibilityId <- generateGUID
          QMgEl.create
            DMgEl.MgEligibility
              { id = eligibilityId,
                campaignId = campaign.id,
                driverId,
                source = DMgEl.DASHBOARD_UPLOAD,
                reason = invitee.reason,
                invitedAt = now,
                notifiedAt = Nothing,
                createdAt = now,
                updatedAt = now
              }
          pure True
      _ -> pure False
  when (campaign.status `elem` [DMgC.PUBLISHED, DMgC.PAUSED]) $ notifyInvitees campaign
  pure Common.MgInviteRes {invited = length (filter (== True) invited), skipped = length (filter not invited)}

-- | Tells every invited driver that has not been told yet. One failed notification does not stop the
-- others; the driver is simply tried again at the next invite or publish.
notifyInvitees :: DMgC.MgCampaign -> Flow ()
notifyInvitees campaign = do
  pending <- filter (isNothing . (.notifiedAt)) <$> QMgEl.findAllByCampaignId campaign.id
  now <- getCurrentTime
  forM_ pending $ \eligibility -> do
    mbDriver <- QP.findById eligibility.driverId
    whenJust mbDriver $ \driver -> do
      res <-
        withTryCatch "minimumGuarantee:notifyInvitee" $
          Notify.notifyDriver
            driver.merchantOperatingCityId
            FCM.DRIVER_NOTIFY
            "Minimum guarantee"
            ("You are invited to \"" <> campaign.name <> "\". Open the app to opt in.")
            driver
            Nothing
      case res of
        Left err -> logError $ "MG invite notification failed for driver " <> eligibility.driverId.getId <> ": " <> show err
        Right () -> QMgEl.updateNotifiedAtById (Just now) eligibility.id

statusLockKey :: Id DCommon.MgCampaign -> Text
statusLockKey campaignId = "MgCampaignStatus:" <> campaignId.getId

postMinimumGuaranteeStatus :: ShortId DM.Merchant -> Context.City -> Id DCommon.MgCampaign -> Common.MgCampaignStatusReq -> Flow APISuccess
postMinimumGuaranteeStatus merchantShortId opCity reqCampaignId req = do
  (merchant, merchantOpCity) <- resolveCity merchantShortId opCity
  -- two admins changing one campaign's status at once must not both act on the same current status
  -- (a double publish would start two settlement chains), so the read and the change are serialised
  Redis.withWaitOnLockRedisWithExpiry (statusLockKey reqCampaignId) 10 60 $ do
    campaign <- findScopedCampaign merchantOpCity (cast reqCampaignId)
    case (campaign.status, req.status) of
      (DMgC.DRAFT, DMgC.PUBLISHED) -> do
        windows <- QMgW.findAllByCampaignId campaign.id
        when (null windows) $ throwError (InvalidRequest "A campaign needs at least one window before it can be published")
        today <- localToday merchantOpCity
        when (campaign.endDate < today) $ throwError (InvalidRequest "endDate is already in the past")
        QMgC.updateStatusById DMgC.PUBLISHED campaign.id
        -- the campaign's own settlement chain, first run in 15 minutes. A published campaign without
        -- its job would never settle and no later request could start it, so a failure here puts the
        -- campaign back to DRAFT and the publish can be retried.
        ( createJobIn @_ @'MinimumGuaranteeSettlement (Just merchant.id) (Just merchantOpCity.id) 900 $
            MinimumGuaranteeSettlementJobData {merchantId = merchant.id, merchantOperatingCityId = merchantOpCity.id, campaignId = campaign.id}
          )
          `C.onException` QMgC.updateStatusById DMgC.DRAFT campaign.id
        notifyInvitees campaign {DMgC.status = DMgC.PUBLISHED}
      (DMgC.PUBLISHED, DMgC.PAUSED) -> do
        QMgC.updateStatusById DMgC.PAUSED campaign.id
        -- drivers inside a window stop being force-assigned for the pause
        setMgCampaignPaused campaign.id True
      (DMgC.PAUSED, DMgC.PUBLISHED) -> do
        QMgC.updateStatusById DMgC.PUBLISHED campaign.id
        setMgCampaignPaused campaign.id False
      (from, DMgC.CANCELLED)
        | from `elem` [DMgC.DRAFT, DMgC.PUBLISHED, DMgC.PAUSED] -> do
          QMgC.updateStatusById DMgC.CANCELLED campaign.id
          setMgCampaignPaused campaign.id False
          -- days that are over are still settled by the campaign's job; today and later are called off
          today <- localToday merchantOpCity
          cancelUpcomingEnrollments campaign today
      (from, to) -> throwError (InvalidRequest $ "transition " <> show from <> " -> " <> show to <> " is not allowed")
  pure Success

postMinimumGuaranteeExtend :: ShortId DM.Merchant -> Context.City -> Id DCommon.MgCampaign -> Common.MgCampaignExtendReq -> Flow APISuccess
postMinimumGuaranteeExtend merchantShortId opCity reqCampaignId req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  campaign <- findScopedCampaign merchantOpCity (cast reqCampaignId)
  unless (campaign.status `elem` [DMgC.PUBLISHED, DMgC.PAUSED]) $
    throwError (InvalidRequest "Only a published or paused campaign can be extended")
  when (req.endDate < campaign.endDate) $ throwError (InvalidRequest "endDate can only be moved later")
  when (maybe False (<= 0) req.maxBudget) $ throwError (InvalidRequest "maxBudget must be positive")
  QMgC.updateEndDateAndMaxBudgetById req.endDate req.maxBudget campaign.id
  pure Success

getMinimumGuaranteeEnrollments :: ShortId DM.Merchant -> Context.City -> Id DCommon.MgCampaign -> Maybe Day -> Flow Common.MgEnrollmentListRes
getMinimumGuaranteeEnrollments merchantShortId opCity reqCampaignId mbDate = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  campaign <- findScopedCampaign merchantOpCity (cast reqCampaignId)
  date <- maybe (localToday merchantOpCity) pure mbDate
  enrollments <- QMgE.findAllByCampaignIdAndPeriodDate campaign.id date
  pure Common.MgEnrollmentListRes {enrollments = map toEnrollmentItem enrollments}

toEnrollmentItem :: DMgE.MgEnrollment -> Common.MgEnrollmentItem
toEnrollmentItem e =
  Common.MgEnrollmentItem
    { driverId = e.driverId.getId,
      periodDate = e.periodDate,
      status = e.status,
      beneficiaryType = e.beneficiaryType,
      beneficiaryId = e.beneficiaryId,
      earned = e.metrics.earned,
      rides = e.metrics.rides,
      distanceKm = e.metrics.distanceMeters `div` 1000,
      breakMinutes = e.metrics.breakSeconds `div` 60,
      cancellations = e.metrics.cancellations,
      payoutPercent = e.payoutPercent,
      topUpAmount = e.topUpAmount,
      ledgerEntryId = e.ledgerEntryId
    }
