module Types.API.CustomerSupport where

-- import Beckn.Types.Amount
-- import Beckn.Types.App
import Beckn.Types.Storage.Case as C
import Beckn.Types.Storage.Location
-- import Beckn.Types.Storage.ProductInstance
-- import Beckn.Types.Storage.Products
import Data.Swagger
import Data.Time
import EulerHS.Prelude

-- {
--      "status": "string", Overide if RIDEORDER present
--      "createdAt": "string",
--      "updatedAt": "string",
--      "fromLocation": "Location",
--      "toLocation": "Location",
--      "startTime": "string",
--      "endTime": "string",
--      "vehicleVariant": "string", -- from where?
--      "trip": Maybe {
--         "id": "string", -- what id is this? ProductInstanceId (RideSearch)
--        "price": "string", -- from where?
--        "status": "string", -- what is this?  ProductInstanceRideOrder status
--        "driver": Maybe "Driver",
--        "vehicle": Maybe "Vehicle",
--        "provider": Maybe "Provider"
--      }
--    }

data Order = Order
  { _id :: Text,
    _status :: CaseStatus,
    _createdAt :: UTCTime,
    _updatedAt :: UTCTime,
    _startTime :: UTCTime,
    _endTime :: Maybe UTCTime,
    _fromLocation :: Maybe Location,
    _toLocation :: Maybe Location,
    _vehicleVariant :: Maybe Text
    -- Add trip
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON Order where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Order where
  toJSON = genericToJSON stripAllLensPrefixOptions
