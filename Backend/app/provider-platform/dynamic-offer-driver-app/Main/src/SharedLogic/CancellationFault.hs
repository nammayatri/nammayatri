{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Phase C of the cancellation unification (see dev/docs/cancellation-fault-verdict-plan.md):
-- one per-city CANCELLATION_FAULT_VERDICT rule pipeline decides who was at fault for a
-- cancellation, evaluated once per ride and cached, so the dues, coin, and tag rules all
-- see the same verdict instead of re-deriving fault independently. Currently ADVISORY:
-- the verdict is fed to downstream rules as input; nothing is enforced in Haskell yet.
--
-- Provenance is the rule's own responsibility: every rule that sets `atFault` MUST also
-- set `rule` — a human-chosen name for itself (e.g. "pickup_stall", "customer_no_show").
-- The mandatory `rule` field in 'FaultVerdict' makes dashboard verification reject rule
-- sets that produce a verdict without naming its source.
module SharedLogic.CancellationFault where

import qualified Data.Aeson as A
import Data.Default.Class
import qualified Domain.Types.CancellationReason as DCancellationReason
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.CancellationSignals as CancellationSignals
import qualified Storage.Queries.Ride as QRide
import Tools.DynamicLogic (getAppDynamicLogic)

data FaultParty = DriverAtFault | CustomerAtFault | SharedFault | NoFault
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

isCustomerAtFault :: Maybe FaultVerdict -> Bool
isCustomerAtFault = maybe False ((== CustomerAtFault) . (.atFault))

isDriverAtFault :: Maybe FaultVerdict -> Bool
isDriverAtFault = maybe False ((== DriverAtFault) . (.atFault))

data FaultVerdict = FaultVerdict
  { atFault :: FaultParty,
    -- MANDATORY: the name the rule author gave the deciding rule inside the JsonLogic
    -- (e.g. "pickup_stall", "customer_no_show"); when later rules in the pipeline
    -- override `atFault`, they must override `rule` too.
    rule :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Default FaultVerdict where
  def = FaultVerdict {atFault = NoFault, rule = noRuleMatched}

noRuleMatched :: Text
noRuleMatched = "NO_RULE_MATCHED"

unnamedRule :: Text
unnamedRule = "UNNAMED_RULE"

-- Input to the CANCELLATION_FAULT_VERDICT rules: who cancelled, the selected reason, and
-- the canonical cancellation signals. Field names are the rule-authoring contract.
data FaultVerdictData = FaultVerdictData
  { cancelledBy :: DCT.CancellationType,
    cancellationReasonSelected :: Maybe DCancellationReason.CancellationReasonCode,
    timeOfCancellation :: Int,
    timeSinceBooking :: Maybe Int,
    isArrivedAtPickup :: Bool,
    driverWaitingTime :: Maybe Int,
    callAttemptByDriver :: Bool,
    callAttemptCount :: Int,
    actualCoveredDistance :: Maybe Meters,
    expectedCoveredDistance :: Maybe Meters,
    initialDistanceToPickup :: Maybe Meters,
    currentDistanceToPickup :: Maybe Meters,
    isAdvanceBooking :: Bool,
    isPickupOrDestinationEdited :: Bool,
    pickupStallCase :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Default FaultVerdictData where
  def =
    FaultVerdictData
      { cancelledBy = DCT.CancellationByCustomer,
        cancellationReasonSelected = Nothing,
        timeOfCancellation = 0,
        timeSinceBooking = Nothing,
        isArrivedAtPickup = False,
        driverWaitingTime = Nothing,
        callAttemptByDriver = False,
        callAttemptCount = 0,
        actualCoveredDistance = Nothing,
        expectedCoveredDistance = Nothing,
        initialDistanceToPickup = Nothing,
        currentDistanceToPickup = Nothing,
        isAdvanceBooking = False,
        isPickupOrDestinationEdited = False,
        pickupStallCase = Nothing
      }

mkFaultVerdictData :: CancellationSignals.CancellationSignals -> DCT.CancellationType -> Maybe DCancellationReason.CancellationReasonCode -> FaultVerdictData
mkFaultVerdictData signals cancelledBy reasonCode =
  FaultVerdictData
    { cancelledBy = cancelledBy,
      cancellationReasonSelected = reasonCode,
      timeOfCancellation = signals.timeOfCancellation,
      timeSinceBooking = signals.timeSinceBooking,
      isArrivedAtPickup = signals.isArrivedAtPickup,
      driverWaitingTime = signals.driverWaitingTime,
      callAttemptByDriver = signals.callAttemptByDriver,
      callAttemptCount = signals.callAttemptCount,
      actualCoveredDistance = signals.actualCoveredDistance,
      expectedCoveredDistance = signals.expectedCoveredDistance,
      initialDistanceToPickup = signals.initialDistanceToPickup,
      currentDistanceToPickup = signals.currentDistanceToPickup,
      isAdvanceBooking = signals.isAdvanceBooking,
      isPickupOrDestinationEdited = signals.isPickupOrDestinationEdited,
      pickupStallCase = signals.pickupStallCase
    }

faultVerdictKey :: Id DRide.Ride -> Text
faultVerdictKey rideId = "CancellationFaultVerdict:rideId-" <> rideId.getId

faultVerdictTtl :: Int
faultVerdictTtl = 3600

type FaultFlow m r = (MonadFlow m, EsqDBFlow m r, CacheFlow m r, ClickhouseFlow m r)

-- | Evaluate the verdict once per ride: first caller computes and caches the outcome;
-- concurrent/later consumers (dues calc, coin event fork) get the cached result.
-- Returns Nothing when the city has no fault rules configured.
getOrComputeFaultVerdict :: FaultFlow m r => DRide.Ride -> Maybe Text -> Seconds -> FaultVerdictData -> m (Maybe FaultVerdict)
getOrComputeFaultVerdict ride mbEntityTxnId timeDiffFromUtc faultData = do
  mbCached :: Maybe FaultVerdict <- Redis.safeGet (faultVerdictKey ride.id)
  case mbCached of
    Just verdict -> pure (Just verdict)
    Nothing -> do
      localTime <- getLocalCurrentTime timeDiffFromUtc
      (logics, _mbVersion) <- getAppDynamicLogic (cast ride.merchantOperatingCityId) LYT.CANCELLATION_FAULT_VERDICT localTime Nothing Nothing
      if null logics
        then pure Nothing
        else do
          verdict <- computeFaultVerdict ride mbEntityTxnId logics faultData
          Redis.setExp (faultVerdictKey ride.id) verdict faultVerdictTtl
          QRide.updateCancellationFaultVerdict (Just $ show verdict.atFault) (Just verdict.rule) ride.id
          pure (Just verdict)

-- | Dry-run twin of 'getOrComputeFaultVerdict' for previews: fetches the same rules and
-- computes the same verdict, but touches NO state — no Redis cache, no ride-row persist
-- (the cancellation may never happen). Nothing when the city has no fault rules.
computeFaultVerdictDryRun :: FaultFlow m r => DRide.Ride -> Maybe Text -> Seconds -> FaultVerdictData -> m (Maybe FaultVerdict)
computeFaultVerdictDryRun ride mbEntityTxnId timeDiffFromUtc faultData = do
  localTime <- getLocalCurrentTime timeDiffFromUtc
  (logics, _mbVersion) <- getAppDynamicLogic (cast ride.merchantOperatingCityId) LYT.CANCELLATION_FAULT_VERDICT localTime Nothing Nothing
  if null logics
    then pure Nothing
    else Just <$> computeFaultVerdict ride mbEntityTxnId logics faultData

computeFaultVerdict :: FaultFlow m r => DRide.Ride -> Maybe Text -> [A.Value] -> FaultVerdictData -> m FaultVerdict
computeFaultVerdict ride mbEntityTxnId logics faultData = do
  resp <- LYDL.runLogicsWithDebugLog LYDL.Driver (cast ride.merchantOperatingCityId) LYT.CANCELLATION_FAULT_VERDICT mbEntityTxnId logics faultData
  verdict <- case A.fromJSON resp.result :: A.Result FaultProbe of
    A.Success probe -> case (probe.atFault, probe.rule) of
      (Nothing, _) -> pure def
      (Just party, Just ruleName) -> pure FaultVerdict {atFault = party, rule = ruleName}
      (Just party, Nothing) -> do
        logError $ "CANCELLATION_FAULT_VERDICT rules set atFault without naming the deciding rule (mandatory `rule` output missing), rideId: " <> ride.id.getId
        pure FaultVerdict {atFault = party, rule = unnamedRule}
    A.Error err -> do
      logError $ "Failed to parse CANCELLATION_FAULT_VERDICT output: " <> show err <> ", rideId: " <> ride.id.getId
      pure def
  logInfo $ "CancellationFaultVerdict for rideId " <> ride.id.getId <> ": " <> show verdict.atFault <> " rule=" <> verdict.rule
  pure verdict

-- internal: extract just the verdict keys from the pipeline result, which is the input
-- object merged with whatever the rules have set (extra keys ignored)
data FaultProbe = FaultProbe
  { atFault :: Maybe FaultParty,
    rule :: Maybe Text
  }
  deriving (Generic, FromJSON)
