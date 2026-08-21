{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Leaf module holding the pickup-stall monitor state and tag constants, split out of
-- SharedLogic.BehaviourManagement.PickupStall (which re-exports it) so that modules
-- deep in the dependency graph (e.g. SharedLogic.CancellationSignals, imported by the
-- coin engine) can read the stall verdict without pulling in the behavior-engine stack.
module SharedLogic.BehaviourManagement.PickupStallState where

import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT

-- Per-ride monitor state kept in Redis between ticks of the CheckDriverPickupProgress job.
-- Lives here (not in the job module) so the customer-cancel flow can read it too.
data PickupProgressState = PickupProgressState
  { lastDistanceToPickup :: Maybe Double,
    candidateCase :: Maybe Text,
    consecutiveBadTicks :: Int,
    activeCase :: Maybe Text,
    caseStartedAt :: Maybe UTCTime,
    firedStageCount :: Int,
    -- Consecutive ticks the scheduled-ride ETA has been infeasible (A > pickup+grace); debounces OSRM noise.
    consecutiveEtaBreaches :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

emptyPickupProgressState :: PickupProgressState
emptyPickupProgressState =
  PickupProgressState
    { lastDistanceToPickup = Nothing,
      candidateCase = Nothing,
      consecutiveBadTicks = 0,
      activeCase = Nothing,
      caseStartedAt = Nothing,
      firedStageCount = 0,
      consecutiveEtaBreaches = 0
    }

pickupProgressStateKey :: Id DRide.Ride -> Text
pickupProgressStateKey rideId = "CheckDriverPickupProgress:rideId-" <> rideId.getId

pickupProgressStateTtl :: Int
pickupProgressStateTtl = 6 * 3600

caseStalled, caseRetreating, caseLocationDark :: Text
caseStalled = "STALLED"
caseRetreating = "RETREATING"
caseLocationDark = "LOCATION_DARK"

pickupStallRideTagPrefix :: Text
pickupStallRideTagPrefix = "PickupStallDetected"

mkPickupStallRideTag :: Text -> LYT.TagNameValue
mkPickupStallRideTag stallCase = LYT.TagNameValue $ pickupStallRideTagPrefix <> "#" <> stallCase
