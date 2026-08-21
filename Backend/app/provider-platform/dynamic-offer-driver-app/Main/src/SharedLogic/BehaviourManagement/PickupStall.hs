{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Single entry point for recording a "driver did not proceed to pickup" event,
-- shared by the pickup progress monitor job (system reallocation) and the customer
-- cancel flow (customer cancelled a ride the driver had stalled on). Repeat-offender
-- consequences (nudge/warn/fee/block) are decided by PICKUP_STALL_BEHAVIOR JsonLogic
-- rules per operating city.
module SharedLogic.BehaviourManagement.PickupStall
  ( module SharedLogic.BehaviourManagement.PickupStall,
    module SharedLogic.BehaviourManagement.PickupStallState,
  )
where

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
import qualified Lib.BehaviorTracker.Recorder as BTRecorder
import qualified Lib.BehaviorTracker.Snapshot as BTSnap
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.Scheduler.Environment (JobCreator)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.BehaviourManagement.ConsequenceDispatcher as BehaviorDispatch
import SharedLogic.BehaviourManagement.PickupStallState
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import Tools.DynamicLogic (getAppDynamicLogic)

pickupStallActionType :: Text
pickupStallActionType = "PICKUP_STALL"

-- Distance-progress helpers shared by the ad-hoc and scheduled monitors (identical logic).

-- Nothing = progressing; Just = bad tick. A road detour can transiently raise straight-line
-- distance, so RETREATING relies on advanceCase's debounce before it activates.
classifyTick :: Maybe Double -> Maybe Double -> Double -> Maybe Text
classifyTick _ Nothing _ = Just caseLocationDark
classifyTick Nothing (Just _) _ = Nothing
classifyTick (Just lastD) (Just currentD) threshold
  | lastD - currentD >= threshold = Nothing
  | currentD - lastD >= threshold = Just caseRetreating
  | otherwise = Just caseStalled

advanceCase :: Int -> Text -> UTCTime -> PickupProgressState -> PickupProgressState
advanceCase debounce detectedCase now state
  | state.activeCase == Just detectedCase = state
  | state.candidateCase == Just detectedCase =
    let count = state.consecutiveBadTicks + 1
     in if count >= debounce
          then state {activeCase = Just detectedCase, caseStartedAt = Just now, firedStageCount = 0, candidateCase = Nothing, consecutiveBadTicks = 0}
          else state {consecutiveBadTicks = count}
  | otherwise = state {candidateCase = Just detectedCase, consecutiveBadTicks = 1, activeCase = Nothing, caseStartedAt = Nothing, firedStageCount = 0}

stagesForCase :: DTC.PickupStallMonitoringConfig -> Text -> [DTC.PickupStallStage]
stagesForCase monitoringConfig caseName
  | caseName == caseStalled = maybe [] (.stages) monitoringConfig.stalledConfig
  | caseName == caseRetreating = maybe [] (.stages) monitoringConfig.retreatingConfig
  | caseName == caseLocationDark = maybe [] (.stages) monitoringConfig.locationDarkConfig
  | otherwise = []

-- Does a stage's terminal action authorize reallocation for this ride? Replaces the old
-- ScheduledReallocationOwner enum: the terminal action scopes reallocation to ad-hoc (REALLOCATE_RIDE),
-- scheduled *intracity* (REALLOCATE_SCHEDULED_RIDE — InterCity/Rental excluded, since isScheduled =
-- startTime>now lumps them in), or every ride (REALLOCATE_ALL_RIDES). Args: action, isScheduled, isInterCityOrRental.
-- Category reallocatability is checked separately (Domain.Types.Trip.isReallocatableCategory).
terminalActionReallocates :: DTC.PickupStallTerminalAction -> Bool -> Bool -> Bool
terminalActionReallocates DTC.RECORD_ONLY _ _ = False
terminalActionReallocates DTC.REALLOCATE_RIDE isScheduled _ = not isScheduled
terminalActionReallocates DTC.REALLOCATE_SCHEDULED_RIDE isScheduled isInterCityOrRental = isScheduled && not isInterCityOrRental
terminalActionReallocates DTC.REALLOCATE_ALL_RIDES _ _ = True

-- Cooldown tags exposed to rules (e.g. {"var": "cooldowns.PICKUP_STALL_FEE"}) so a
-- fee/block consequence fires at most once per cooldown window.
pickupStallCooldownTags :: [Text]
pickupStallCooldownTags = ["PICKUP_STALL_FEE", "PICKUP_STALL_BLOCK"]

pickupStallCounterConfig :: BTT.CounterConfig
pickupStallCounterConfig =
  BTT.CounterConfig
    { windowSizeDays = 30,
      counters = [BTT.ACTION_COUNT],
      periods = [BTT.mkPeriodConfig "daily" 1, BTT.mkPeriodConfig "weekly" 7, BTT.mkPeriodConfig "monthly" 30]
    }

data PickupStallSource = SystemReallocation | SystemDetection | CustomerCancelledDriverAtFault
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

recordPickupStall ::
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
  Text ->
  PickupStallSource ->
  m ()
recordPickupStall transporterConfig driverId merchantOpCityId rideId stallCase source = do
  BTRecorder.incrementCounterOnly pickupStallCounterConfig BTT.DRIVER driverId.getId pickupStallActionType BTT.ACTION_COUNT
  eventTime <- getCurrentTime
  let actionEvent =
        BTT.ActionEvent
          { entityType = BTT.DRIVER,
            entityId = driverId.getId,
            actionType = pickupStallActionType,
            merchantOperatingCityId = merchantOpCityId.getId,
            flowContext = A.object [],
            eventData =
              A.object
                [ "stallCase" A..= stallCase,
                  "source" A..= (show source :: Text),
                  "rideId" A..= rideId.getId
                ],
            timestamp = eventTime
          }
  snapshot <- BTSnap.buildSnapshotWithCooldowns pickupStallCounterConfig actionEvent (A.object []) pickupStallCooldownTags
  let fetchRules = \dom -> do
        localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
        getAppDynamicLogic (cast merchantOpCityId) dom localTime Nothing Nothing
  output <- BEOrch.orchestrate snapshot LYDL.Driver (cast merchantOpCityId) LYT.PICKUP_STALL_BEHAVIOR fetchRules
  logInfo $ "PickupStallBehavior for driver " <> driverId.getId <> " (" <> stallCase <> ", " <> show source <> "): consequences=" <> show (length output.consequences) <> ", communications=" <> show (length output.communications)
  when (not (null output.consequences) || not (null output.communications)) $ do
    let dispatchCtx =
          BehaviorDispatch.DispatchContext
            { merchantId = transporterConfig.merchantId,
              merchantOperatingCityId = merchantOpCityId,
              counterConfig = Just pickupStallCounterConfig,
              actionEvent = Just actionEvent
            }
    BehaviorDispatch.handleConsequences dispatchCtx driverId output.consequences
    BehaviorDispatch.handleCommunications driverId output.communications
