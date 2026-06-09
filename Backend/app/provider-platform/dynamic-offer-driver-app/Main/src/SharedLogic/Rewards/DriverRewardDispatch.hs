{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Rewards.DriverRewardDispatch
  ( tryDispatchDriverRewards,
  )
where

import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleVariant as DTVeh
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import Lib.Scheduler.Environment (JobCreator)
import Lib.Scheduler.JobStorageType.SchedulerType ()
import Lib.Yudhishthira.Storage.Beam.BeamFlow (HasYudhishthiraTablesSchema)
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.BehaviourManagement.ConsequenceDispatcher as BehaviorDispatch
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import qualified SharedLogic.Rewards.RewardPipeline as RewardPipeline
import Tools.Metrics (CoreMetrics)

-- | Runs DRIVER_REWARDS json-logic evaluation and dispatches consequences when matched.
-- Returns True when rewards were handled (caller should skip legacy coin/wallet path).
tryDispatchDriverRewards ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    ClickhouseFlow m r,
    HasYudhishthiraTablesSchema,
    Hedis.HedisFlow m r,
    MonadReader r m,
    Hedis.HedisLTSFlowEnv r,
    CoreMetrics m,
    HasLocationService m r,
    JobCreator r m,
    HasShortDurationRetryCfg r c
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
  m Bool
tryDispatchDriverRewards driverId mbDriver merchantId merchantOpCityId eventType entityId mbVehVariant mbServiceTierType mbConfigVersionMap = do
  mbRewardEval <-
    RewardPipeline.evaluateDriverRewards driverId mbDriver merchantId merchantOpCityId eventType entityId mbVehVariant mbServiceTierType mbConfigVersionMap
  case mbRewardEval of
    Nothing -> pure False
    Just RewardPipeline.DriverRewardEvalResult {..} -> do
      let dispatchCtx =
            BehaviorDispatch.DispatchContext
              { merchantId,
                merchantOperatingCityId = merchantOpCityId,
                counterConfig = Just counterConfig,
                actionEvent = Just actionEvent,
                rewardContext = Just rewardCtx
              }
      BehaviorDispatch.handleConsequences dispatchCtx driverId output.consequences
      BehaviorDispatch.handleCommunications driverId output.communications
      logInfo $ "Driver rewards handled via DRIVER_REWARDS json logic for driver " <> driverId.getId
      pure True
