module DriverOfferBPP.Types where

import Data.Time
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong)
import Kernel.Types.Id (Id)

data DriverAvailability = DriverAvailability
  { id :: Id DriverAvailability,
    driverId :: Text,
    merchantId :: Text,
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
