{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Consumer.AvailabilityTime.Types where

import Data.OpenApi
import Data.Time
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong)
import Kernel.Types.Id (Id)

data DriverAvailability = DriverAvailability
  { id :: Id DriverAvailability,
    driverId :: DriverId,
    merchantId :: MerchantId,
    totalAvailableTime :: Int,
    lastAvailableTime :: UTCTime,
    bucketStartTime :: UTCTime,
    bucketEndTime :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show)

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

type BucketTimePair = (UTCTime, UTCTime)

type MerchantId = Text

type DriverId = Text

type LastAvailableTime = UTCTime

type SecondsActiveInBucket = Integer

type AvailabilityBucket = Map BucketTimePair (SecondsActiveInBucket, LastAvailableTime)
