{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Payload type of mg_enrollment.metrics (Minimum Guarantee campaigns). Hand-written because it is
-- one plain record field of the table: the generator turns such a field into one column per record
-- field, whereas this is stored as ONE JSON text value via the spec's toTType/fromTType codec.
module Domain.Types.Extra.MgEnrollment where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

-- | Counters of one driver's day. cancellations and breakSeconds are event-based; the rest are summed
-- at end-ride from rides whose tripStartTime fell inside a window.
data MgMetrics = MgMetrics
  { -- driver-fault cancellations inside a window
    cancellations :: Int,
    -- offline/silent seconds inside the windows, closed breaks only
    breakSeconds :: Int,
    -- start of the break in progress, if any
    breakStartedAt :: Maybe UTCTime,
    rides :: Int,
    distanceMeters :: Int,
    -- sum of counted rides' fares (tips and tolls excluded)
    earned :: HighPrecMoney,
    -- rides already added above; makes a retried ride-ended event a no-op
    countedRideIds :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | A day's counters before anything has happened.
emptyMgMetrics :: MgMetrics
emptyMgMetrics =
  MgMetrics
    { cancellations = 0,
      breakSeconds = 0,
      breakStartedAt = Nothing,
      rides = 0,
      distanceMeters = 0,
      earned = 0,
      countedRideIds = []
    }
