module Types.Storage.Ride where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Types.Storage.ProductInstance (ProductInstance)
import qualified Types.Storage.Vehicle as SVeh

data RideStatus = NEW | INPROGRESS | COMPLETED | CANCELLED
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data RideAPIEntity = RideAPIEntity
  { id :: Id ProductInstance,
    shortRideId :: ShortId ProductInstance,
    status :: RideStatus,
    driverName :: Maybe Text,
    driverNumber :: Maybe Text,
    vehicleNumber :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleVariant :: Maybe SVeh.Variant,
    vehicleModel :: Maybe Text,
    computedPrice :: Maybe Amount,
    actualRideDistance :: Maybe Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, FromJSON, ToJSON, Generic)
