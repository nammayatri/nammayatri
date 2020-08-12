module Types.Common where

import Data.Time (LocalTime)
import EulerHS.Prelude

data GPS = GPS
  { lat :: Text,
    lon :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Address = Address
  { door :: Text,
    building :: Text,
    street :: Text,
    area :: Text,
    city :: Text,
    country :: Text,
    areaCode :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype City = City Text
  deriving (Generic, Show, FromJSON, ToJSON)

data Location = Location
  { locType :: Text,
    gps :: Maybe GPS,
    address :: Maybe Address,
    areaCode :: Maybe Text,
    city :: Maybe City
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data StopTime = StopTime
  { estimated :: LocalTime,
    actual :: Maybe LocalTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Stop = Stop
  { location :: Location,
    arrivalTime :: StopTime,
    departureTime :: StopTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data VehicleCategory = CAR | MOTORCYCLE | BICYCLE | TRUCK | OTHER
  deriving (Generic, FromJSON, ToJSON, Show)

data Vehicle = Vehicle
  { category :: Maybe VehicleCategory,
    capacity :: Maybe Int,
    model :: Maybe Text,
    variant :: Text,
    registrationNumber :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data DecimalValue = DecimalValue
  { integral :: Text,
    fractional :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Traveller = Traveller
  { name :: Text,
    gender :: Text,
    phones :: [Text]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype Tracking = Tracking
  { url :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Driver = Driver
  { name :: Text,
    gender :: Text,
    phones :: [Text]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Trip = Trip
  { id :: Text,
    origin :: Maybe Stop,
    destination :: Maybe Stop,
    vehicle :: Maybe Vehicle,
    driver :: Maybe Driver,
    travellers :: [Traveller],
    fare :: Maybe DecimalValue
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Provider = Provider
  { id :: Text,
    name :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
