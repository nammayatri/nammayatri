{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Consumer.AvailabilityTime.Types where

import Data.Time
import EulerHS.Prelude hiding (id)
import Kernel.Types.CommonImport (LatLong)
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

data LocationUpdates = LocationUpdates
  { rId :: Maybe Text,
    ts :: UTCTime,
    pt :: LatLong,
    mId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

type BucketTimePair = (UTCTime, UTCTime)

type MerchantId = Text

type DriverId = Text

type LastAvailableTime = UTCTime

type SecondsActiveInBucket = Integer

type AvailabilityBucket = Map BucketTimePair (SecondsActiveInBucket, LastAvailableTime)
