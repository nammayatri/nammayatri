{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Records a customer rating of a driver and runs the city's RATING_BEHAVIOR rules.
module SharedLogic.BehaviourManagement.LowRating where

import qualified Data.Aeson as A
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorEngine.Orchestrator as BEOrch
import qualified Lib.BehaviorTracker.Snapshot as BTSnap
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.Scheduler.Environment (JobCreator)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.BehaviourManagement.ConsequenceDispatcher as BehaviorDispatch
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import Tools.DynamicLogic (getAppDynamicLogic)

lowRatingActionType :: Text
lowRatingActionType = "LOW_RATING"

-- Ratings are sparse compared to cancellations, so the windows are longer:
-- rules should gate on monthly/quarterly counters, not daily ones.
lowRatingCounterConfig :: BTT.CounterConfig
lowRatingCounterConfig =
  BTT.CounterConfig
    { windowSizeDays = 90,
      counters = [BTT.ACTION_COUNT, BTT.ELIGIBLE_COUNT],
      periods = [BTT.mkPeriodConfig "weekly" 7, BTT.mkPeriodConfig "monthly" 30, BTT.mkPeriodConfig "quarterly" 90],
      hashTagEntityId = False
    }

-- The snapshot counts exclude this rating; rules count it with INCREMENT_COUNTER
-- (ELIGIBLE_COUNT for every new rating, ACTION_COUNT when they deem it low).
recordDriverRating ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    HasLocationService m r,
    JobCreator r m,
    Redis.HedisLTSFlowEnv r,
    HasShortDurationRetryCfg r c,
    ClickhouseFlow m r
  ) =>
  DTC.TransporterConfig ->
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  Id DRide.Ride ->
  Int ->
  Bool ->
  Maybe Centesimal ->
  Int ->
  m ()
recordDriverRating transporterConfig driverId merchantOpCityId rideId ratingValue isNewRating lifetimeRating lifetimeRatingCount = do
  eventTime <- getCurrentTime
  let actionEvent =
        BTT.ActionEvent
          { entityType = BTT.DRIVER,
            entityId = driverId.getId,
            actionType = lowRatingActionType,
            merchantOperatingCityId = merchantOpCityId.getId,
            flowContext = A.object [],
            eventData =
              A.object
                [ "ratingValue" A..= ratingValue,
                  "rideId" A..= rideId.getId,
                  "isNewRating" A..= isNewRating
                ],
            timestamp = eventTime
          }
      entityState =
        A.object
          [ "lifetimeRating" A..= lifetimeRating,
            "lifetimeRatingCount" A..= lifetimeRatingCount
          ]
  snapshot <- BTSnap.buildSnapshot lowRatingCounterConfig actionEvent entityState
  let fetchRules = \dom -> do
        localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
        getAppDynamicLogic (cast merchantOpCityId) dom localTime Nothing Nothing
  output <- BEOrch.orchestrate snapshot LYDL.Driver (cast merchantOpCityId) LYT.RATING_BEHAVIOR fetchRules
  logInfo $ "RatingBehavior for driver " <> driverId.getId <> " (rating " <> show ratingValue <> "): consequences=" <> show (length output.consequences) <> ", communications=" <> show (length output.communications)
  when (not (null output.consequences) || not (null output.communications)) $ do
    let dispatchCtx =
          BehaviorDispatch.DispatchContext
            { merchantId = transporterConfig.merchantId,
              merchantOperatingCityId = merchantOpCityId,
              counterConfig = Just lowRatingCounterConfig,
              actionEvent = Just actionEvent
            }
    BehaviorDispatch.handleConsequences dispatchCtx driverId output.consequences
    BehaviorDispatch.handleCommunications dispatchCtx driverId output.communications
