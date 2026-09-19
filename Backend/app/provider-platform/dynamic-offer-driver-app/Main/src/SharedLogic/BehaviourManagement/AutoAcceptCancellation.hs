{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Records a driver cancel of an auto-accepted ride and runs the city's rules for it.
module SharedLogic.BehaviourManagement.AutoAcceptCancellation where

import qualified Data.Aeson as A
import Domain.Types.Common (ServiceTierType)
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
import qualified SharedLogic.CancellationFault as CancellationFault
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import Tools.DynamicLogic (getAppDynamicLogic)

autoAcceptCancellationActionType :: Text
autoAcceptCancellationActionType = "AUTO_ACCEPT_CANCELLATION"

autoAcceptCancellationCounterConfig :: BTT.CounterConfig
autoAcceptCancellationCounterConfig =
  BTT.CounterConfig
    { windowSizeDays = 30,
      counters = [BTT.ACTION_COUNT],
      periods = [BTT.mkPeriodConfig "daily" 1, BTT.mkPeriodConfig "weekly" 7, BTT.mkPeriodConfig "monthly" 30],
      hashTagEntityId = False
    }

-- The snapshot count excludes this cancel; rules add it with INCREMENT_COUNTER.
recordAutoAcceptCancellation ::
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
  ServiceTierType ->
  CancellationFault.FaultVerdictData ->
  Maybe CancellationFault.FaultVerdict ->
  m ()
recordAutoAcceptCancellation transporterConfig driverId merchantOpCityId rideId serviceTier verdictData mbVerdict = do
  eventTime <- getCurrentTime
  let actionEvent =
        BTT.ActionEvent
          { entityType = BTT.DRIVER,
            entityId = driverId.getId,
            actionType = autoAcceptCancellationActionType,
            merchantOperatingCityId = merchantOpCityId.getId,
            flowContext = A.object [],
            eventData =
              A.object
                [ "serviceTier" A..= (show serviceTier :: Text),
                  "rideId" A..= rideId.getId,
                  "faultVerdict" A..= fmap (.atFault) mbVerdict,
                  "faultRule" A..= fmap (.rule) mbVerdict,
                  "signals" A..= verdictData
                ],
            timestamp = eventTime
          }
  snapshot <- BTSnap.buildSnapshot autoAcceptCancellationCounterConfig actionEvent (A.object [])
  let fetchRules = \dom -> do
        localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
        getAppDynamicLogic (cast merchantOpCityId) dom localTime Nothing Nothing
  output <- BEOrch.orchestrate snapshot LYDL.Driver (cast merchantOpCityId) LYT.AUTO_ACCEPT_CANCELLATION_BEHAVIOR fetchRules
  logInfo $ "AutoAcceptCancellationBehavior for driver " <> driverId.getId <> " (" <> show serviceTier <> "): consequences=" <> show (length output.consequences) <> ", communications=" <> show (length output.communications)
  when (not (null output.consequences) || not (null output.communications)) $ do
    let dispatchCtx =
          BehaviorDispatch.DispatchContext
            { merchantId = transporterConfig.merchantId,
              merchantOperatingCityId = merchantOpCityId,
              counterConfig = Just autoAcceptCancellationCounterConfig,
              actionEvent = Just actionEvent
            }
    BehaviorDispatch.handleConsequences dispatchCtx driverId output.consequences
    BehaviorDispatch.handleCommunications dispatchCtx driverId output.communications
