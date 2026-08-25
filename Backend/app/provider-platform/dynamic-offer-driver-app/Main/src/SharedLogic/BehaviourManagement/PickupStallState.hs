{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Leaf module holding the pickup progress monitor state, split out of
-- SharedLogic.BehaviourManagement.PickupStall (which re-exports it) so that modules
-- deep in the dependency graph (e.g. SharedLogic.CancellationSignals, imported by the
-- coin engine) can read the journey without pulling in the behavior-engine stack.
--
-- The model is a single progress clock, not a per-case state machine: `bestDistance`
-- only ever improves, `faultSeconds` accumulates whenever we have fresh-location
-- evidence of no progress (across STALLED and MOVING_AWAY alike, so switching between
-- fault kinds never resets escalation), and GPS-dark time is judgment-pending — resolved
-- retroactively by the next fix (forgiven if the driver reappears closer than
-- `bestDistance`, counted into `faultSeconds` otherwise).
module SharedLogic.BehaviourManagement.PickupStallState where

import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Ride as QRide

-- Per-ride monitor state kept in Redis between ticks of the CheckDriverPickupProgress job.
data PickupProgressState = PickupProgressState
  { bestDistance :: Maybe Double, -- closest approach to pickup so far; never worsens
    lastFixLat :: Maybe Double,
    lastFixLon :: Maybe Double,
    lastTickAt :: Maybe UTCTime,
    faultSeconds :: Int, -- cumulative hard no-progress time over the whole pickup phase
    darkSince :: Maybe UTCTime, -- set while GPS dark; cleared (and judged) on the next fresh fix
    detourCreditUsedSec :: Int,
    behaviour :: DRide.PickupBehaviour,
    firedStageCount :: Int,
    firedDarkStageCount :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

emptyPickupProgressState :: PickupProgressState
emptyPickupProgressState =
  PickupProgressState
    { bestDistance = Nothing,
      lastFixLat = Nothing,
      lastFixLon = Nothing,
      lastTickAt = Nothing,
      faultSeconds = 0,
      darkSince = Nothing,
      detourCreditUsedSec = 0,
      behaviour = DRide.PROGRESSING,
      firedStageCount = 0,
      firedDarkStageCount = 0
    }

pickupProgressStateKey :: Id DRide.Ride -> Text
pickupProgressStateKey rideId = "CheckDriverPickupProgress:rideId-" <> rideId.getId

pickupProgressStateTtl :: Int
pickupProgressStateTtl = 6 * 3600

-- Engine defaults; each is overridable per city via the optional advanced fields of
-- PickupStallMonitoringConfig (staleness default is 2 * tickIntervalSec, see the job).
defaultProgressThresholdMeters :: Int
defaultProgressThresholdMeters = 50

defaultDeviationAllowanceMeters :: Int
defaultDeviationAllowanceMeters = 250

defaultDetourDisplacementMeters :: Int
defaultDetourDisplacementMeters = 100

defaultDetourCreditSec :: Int
defaultDetourCreditSec = 240

behaviourLabel :: DRide.PickupBehaviour -> Text
behaviourLabel = show

-- | The pickup-phase summary every cancellation consumer reasons over: what the driver
-- is doing now (or was doing when monitoring ended), how much of the phase he provably
-- wasted, and how long he has been unresolved-dark.
data PickupJourney = PickupJourney
  { behaviour :: DRide.PickupBehaviour,
    faultSeconds :: Int,
    darkSeconds :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Live view of the monitor state; `Nothing` when monitoring never ran (not configured,
-- OTP/meter ride) and nothing was flushed.
getPickupJourney :: (MonadFlow m, CacheFlow m r) => DRide.Ride -> m (Maybe PickupJourney)
getPickupJourney ride = case ride.pickupBehaviour of
  Just flushedBehaviour ->
    pure $
      Just
        PickupJourney
          { behaviour = flushedBehaviour,
            faultSeconds = fromMaybe 0 ride.pickupFaultSeconds,
            darkSeconds = fromMaybe 0 ride.pickupDarkSeconds
          }
  Nothing -> do
    mbState :: Maybe PickupProgressState <- Redis.safeGet (pickupProgressStateKey ride.id)
    forM mbState $ \state -> do
      now <- getCurrentTime
      -- extend the accrued clocks by the span since the last tick, so a cancel landing
      -- between ticks sees up-to-the-moment numbers
      let sinceLastTick = maybe 0 (\t -> max 0 . round $ diffUTCTime now t) state.lastTickAt
          liveFaultSeconds =
            state.faultSeconds
              + if isNothing state.darkSince && state.behaviour `elem` [DRide.STALLED, DRide.MOVING_AWAY] then sinceLastTick else 0
          liveDarkSeconds = maybe 0 (max 0 . round . diffUTCTime now) state.darkSince
      pure PickupJourney {behaviour = state.behaviour, faultSeconds = liveFaultSeconds, darkSeconds = liveDarkSeconds}

-- | Persist the journey onto the ride and drop the Redis state. Called wherever
-- monitoring ends: driver arrival / ride start (behaviour override REACHED_PICKUP),
-- terminal stage, and both cancel flows. No-op when monitoring never ran.
flushPickupJourney :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DRide.Ride -> Maybe DRide.PickupBehaviour -> m ()
flushPickupJourney ride mbBehaviourOverride = do
  mbJourney <- getPickupJourney ride
  whenJust mbJourney $ \journey -> do
    when (isNothing ride.pickupBehaviour) $
      QRide.updatePickupJourney (Just $ fromMaybe journey.behaviour mbBehaviourOverride) (Just journey.faultSeconds) (Just journey.darkSeconds) ride.id
    Redis.del (pickupProgressStateKey ride.id)
