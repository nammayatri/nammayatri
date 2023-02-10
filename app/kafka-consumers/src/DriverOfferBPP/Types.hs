module DriverOfferBPP.Types where

import Data.Time
import EulerHS.Prelude hiding (id)
import qualified Kafka.Consumer as C
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

type BucketTimePair = (UTCTime, UTCTime)

type MerchantId = Text

type DriverId = Text

type LastAvailableTime = UTCTime

type SecondsActiveInBucket = Integer

type AvailabilityBucket = Map BucketTimePair (SecondsActiveInBucket, LastAvailableTime)

type ConsumerRecordD = C.ConsumerRecord (Maybe ByteString) (Maybe ByteString)
