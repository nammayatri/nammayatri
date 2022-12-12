module DriverOfferBPP.Types where

import Beckn.External.Maps.Types (LatLong)
import Beckn.Types.Id (Id)
import Data.Time
import EulerHS.Prelude hiding (id)

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

data LocationUpdates = LocationUpdates
  { rId :: Maybe Text,
    ts :: UTCTime,
    pt :: LatLong,
    mId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
