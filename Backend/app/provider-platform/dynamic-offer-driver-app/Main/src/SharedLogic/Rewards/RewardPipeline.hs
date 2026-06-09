{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Rewards.RewardPipeline
  ( evaluateDriverRewards,
    DriverRewardEvalResult (..),
  )
where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardOffer as DReward
import qualified Domain.Types.VehicleVariant as DTVeh
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.BehaviorEngine.Orchestrator as BEOrch
import Lib.BehaviorEngine.Types (OrchestratedOutput)
import qualified Lib.BehaviorTracker.Snapshot as BTSnap
import qualified Lib.BehaviorTracker.Types as BTT
import qualified Lib.DriverCoins.Types as DCT
import Lib.Yudhishthira.Storage.Beam.BeamFlow (HasYudhishthiraTablesSchema)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.Rewards.Types as RewardTypes
import Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.RewardOffer as CQRewardOffer
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RewardOffer as QRewardOffer
import Tools.DynamicLogic (getAppDynamicLogic)
import Tools.Error
import Utils.Common.Cac.KeyNameConstants

data DriverRewardEvalResult = DriverRewardEvalResult
  { output :: OrchestratedOutput,
    rewardCtx :: RewardTypes.RewardDispatchContext,
    counterConfig :: BTT.CounterConfig,
    actionEvent :: BTT.ActionEvent
  }

evaluateDriverRewards ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    ClickhouseFlow m r,
    HasYudhishthiraTablesSchema,
    Hedis.HedisFlow m r
  ) =>
  Id DP.Person ->
  Maybe DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DCT.DriverCoinsEventType ->
  Maybe Text ->
  Maybe DTVeh.VehicleVariant ->
  Maybe DTC.ServiceTierType ->
  Maybe [LYT.ConfigVersionMap] ->
  m (Maybe DriverRewardEvalResult)
evaluateDriverRewards driverId mbPerson _merchantId merchantOpCityId eventType entityId mbVehVariant mbServiceTierType mbConfigVersionMap = do
  person <- case mbPerson of
    Just p -> pure p
    Nothing -> QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let driverTagTexts = tagTextsFromPerson person
  if null driverTagTexts
    then pure Nothing
    else do
      transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      case QRewardOffer.rewardTriggerEventFromDriverCoinsEvent (T.pack $ show eventType) of
        Nothing -> do
          logDebug $ "No trigger event found for event type: " <> show eventType
          pure Nothing
        Just triggerEvent -> do
          offers <- CQRewardOffer.findAllActiveByMerchantOpCityId merchantOpCityId
          let matchingOffers =
                filter
                  ( \o ->
                      o.entityType == DReward.DRIVER
                        && o.triggerEvent == triggerEvent
                        && tagsMatch driverTagTexts o.requiredTags
                  )
                  offers
          if null matchingOffers
            then do
              logDebug $ "No matching offers found for event type: " <> show eventType
              pure Nothing
            else do
              now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
              (logics, _) <- getAppDynamicLogic (cast merchantOpCityId) LYT.DRIVER_REWARDS now Nothing Nothing
              if null logics
                then do
                  logDebug $ "No logics found for event type: " <> show eventType
                  pure Nothing
                else do
                  let vehCategory = maybe (DTVeh.getVehicleCategoryFromVehicleVariantDefault mbVehVariant) DTVeh.castServiceTierToVehicleCategory mbServiceTierType
                  validRideCount <- getValidRideCountForEvent driverId eventType
                  eventTime <- getCurrentTime
                  let actionEvent =
                        BTT.ActionEvent
                          { entityType = BTT.DRIVER,
                            entityId = driverId.getId,
                            actionType = T.pack $ show eventType,
                            merchantOperatingCityId = merchantOpCityId.getId,
                            flowContext = A.object ["matchingOfferIds" A..= map (getId . (.id)) matchingOffers],
                            eventData =
                              A.object
                                [ "validRideCount" A..= validRideCount,
                                  "triggerEvent" A..= A.toJSON triggerEvent
                                ],
                            timestamp = eventTime
                          }
                      entityState =
                        A.object
                          [ "driverTags" A..= driverTagTexts,
                            "vehicleCategory" A..= A.toJSON vehCategory
                          ]
                  let counterConfig = BTT.CounterConfig {windowSizeDays = 365, counters = [], periods = []}
                  snapshot <- BTSnap.buildSnapshotWithCooldowns counterConfig actionEvent entityState []
                  let fetchRules dom = getAppDynamicLogic (cast merchantOpCityId) dom now Nothing Nothing
                  output <- BEOrch.orchestrate snapshot LYDL.Driver (cast merchantOpCityId) LYT.DRIVER_REWARDS fetchRules
                  if null output.consequences && null output.communications
                    then do
                      logDebug $ "No consequences or communications found for event type: " <> show eventType
                      pure Nothing
                    else do
                      let rewardCtx =
                            RewardTypes.RewardDispatchContext
                              { eventType,
                                entityId,
                                vehCategory,
                                mbServiceTierType,
                                mbConfigVersionMap,
                                transporterConfig,
                                mbFleetOwnerId = Nothing
                              }
                      pure $
                        Just
                          DriverRewardEvalResult
                            { output,
                              rewardCtx,
                              counterConfig,
                              actionEvent
                            }
  where
    tagTextsFromPerson p = [raw | LYT.TagNameValueExpiry raw <- fromMaybe [] p.driverTag]

getValidRideCountForEvent :: (MonadFlow m, Hedis.HedisFlow m r) => Id DP.Person -> DCT.DriverCoinsEventType -> m Int
getValidRideCountForEvent driverId eventType = case eventType of
  DCT.EndRide {tripCategoryType} -> case tripCategoryType of
    DCT.OTPRideTrip ->
      fromMaybe 0 <$> Hedis.runInMasterCloudRedisCellWithCrossAppRedis (Hedis.get $ "DriverOTPValidRideCount:DriverId:" <> driverId.getId)
    DCT.DynamicOfferTrip ->
      fromMaybe 0 <$> Hedis.runInMasterCloudRedisCellWithCrossAppRedis (Hedis.get $ "DriverValidRideCount:DriverId:" <> driverId.getId)
  _ -> pure 0

tagsMatch :: [Text] -> [Text] -> Bool
tagsMatch driverTags requiredTags =
  null requiredTags || any (`elem` driverTags) requiredTags
