{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Processor.LocationUpdate.Types where

import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude

type DriverId = Text

data RideStatus
  = ON_RIDE
  | ON_PICKUP
  | IDLE
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationUpdates = LocationUpdates
  { r_id :: Maybe Text,
    ts :: UTCTime,
    mocid :: Maybe Text,
    st :: Maybe UTCTime,
    pt :: LatLong,
    acc :: Maybe Double,
    ride_status :: RideStatus,
    speed :: Maybe Double,
    da :: Maybe Bool,
    mode :: Maybe Text,
    m_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype DriverIdTokenKey = DriverIdTokenKey
  { driverId :: DriverId
  }
  deriving (Generic, FromJSON, ToJSON, Show)

-- | Wire format used by the Redis-Stream transport. Bundles the location
-- update with the driver id (which Kafka carries in the message key).
data LocationEntry = LocationEntry
  { driverId :: DriverId,
    locationUpdate :: LocationUpdates
  }
  deriving (Generic, FromJSON, ToJSON, Show)
