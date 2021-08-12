{-# LANGUAGE OverloadedLabels #-}

module Types.Common where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Address as Address
import qualified Beckn.Types.Core.Category as Category
import qualified Beckn.Types.Core.DecimalValue as DV
import qualified Beckn.Types.Core.Descriptor as Descriptor
import qualified Beckn.Types.Core.Location as Location
import qualified Beckn.Types.Core.Person as Person
import qualified Beckn.Types.Core.Price as Price
import Beckn.Types.Core.Rating (Rating)
import qualified Beckn.Types.Core.Tag as Tag
import qualified Beckn.Types.Core.Tracking as Tracking
import qualified Beckn.Types.Mobility.Driver as Driver
import qualified Beckn.Types.Mobility.Payload as Payload
import qualified Beckn.Types.Mobility.Stop as Stop
import qualified Beckn.Types.Mobility.Traveller as Traveller
import qualified Beckn.Types.Mobility.Trip as Trip
import qualified Beckn.Types.Mobility.Vehicle as Vehicle
import Control.Lens.Prism (_Just)
import qualified Data.Text as T
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (drop, id, state)

data GPS = GPS
  { lat :: Text,
    lon :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data Address = Address
  { door :: Text,
    building :: Text,
    street :: Text,
    area :: Text,
    city :: Text,
    country :: Text,
    areaCode :: Text,
    state :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype City = City Text
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data Location = Location
  { gps :: Maybe GPS,
    address :: Maybe Address,
    city :: Maybe City
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data StopTime = StopTime
  { estimated :: UTCTime,
    actual :: Maybe UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data Stop = Stop
  { location :: Location,
    arrivalTime :: StopTime,
    departureTime :: StopTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data VehicleCategory = CAR | MOTORCYCLE | BICYCLE | TRUCK | AUTO_RICKSHAW | OTHER
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data Vehicle = Vehicle
  { category :: Maybe VehicleCategory,
    capacity :: Maybe Int,
    make :: Maybe Text,
    model :: Maybe Text,
    size :: Maybe Text,
    variant :: Text,
    color :: Maybe Text,
    registrationNumber :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data DecimalValue = DecimalValue
  { integral :: Text,
    fractional :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data Traveller = Traveller
  { name :: Text,
    gender :: Text,
    phones :: [Text]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype Tracking = Tracking
  { url :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Driver = Driver
  { name :: Text,
    gender :: Text,
    phones :: [Text],
    rating :: Maybe Rating,
    registeredAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data Trip = Trip
  { id :: Text,
    pickup :: Maybe Stop,
    drop :: Maybe Stop,
    vehicle :: Maybe Vehicle,
    driver :: Maybe Driver,
    travellers :: [Traveller],
    fare :: Maybe DecimalValue
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Provider = Provider
  { id :: Text,
    name :: Maybe Text,
    phones :: [Text],
    info :: Maybe ProviderStats
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ProviderStats = ProviderStats
  { completed :: Maybe Int,
    inprogress :: Maybe Int,
    confirmed :: Maybe Int
  }
  deriving (Generic, Read, FromJSON, ToJSON, Show, ToSchema)

instance FromBeckn Location.City City where
  fromBeckn city = City $ city.name

instance ToBeckn Location.City City where
  toBeckn (City cityName) =
    Location.City
      { name = cityName,
        code = ""
      }

instance FromBeckn Text VehicleCategory where
  fromBeckn category =
    case category of
      "CAR" -> CAR
      "MOTORCYCLE" -> MOTORCYCLE
      "BICYCLE" -> BICYCLE
      "OTHER" -> OTHER
      "TRUCK" -> TRUCK
      "AUTO-RICKSHAW" -> AUTO_RICKSHAW
      _ -> OTHER

instance ToBeckn Text VehicleCategory where
  toBeckn category =
    case category of
      CAR -> "CAR"
      MOTORCYCLE -> "MOTORCYCLE"
      BICYCLE -> "BICYCLE"
      OTHER -> "OTHER"
      TRUCK -> "TRUCK"
      AUTO_RICKSHAW -> "AUTO-RICKSHAW"

instance FromBeckn Vehicle.Vehicle Vehicle where
  fromBeckn vehicle =
    Vehicle
      { category = fromBeckn <$> vehicle.category,
        capacity = vehicle.capacity,
        make = vehicle.make,
        model = vehicle.model,
        size = vehicle.size,
        variant = vehicle.variant,
        color = vehicle.color,
        registrationNumber = Vehicle.number <$> (vehicle.registration)
      }

instance ToBeckn Vehicle.Vehicle Vehicle where
  toBeckn vehicle =
    Vehicle.Vehicle
      { category = toBeckn <$> vehicle.category,
        capacity = vehicle.capacity,
        make = vehicle.make,
        model = vehicle.model,
        size = vehicle.size,
        variant = vehicle.variant,
        color = vehicle.color,
        energy_type = Nothing,
        registration = Nothing
      }

instance FromBeckn Location.GPS GPS where
  fromBeckn gps =
    GPS
      { lat = gps.lat,
        lon = gps.lon
      }

instance ToBeckn Location.GPS GPS where
  toBeckn gps =
    Location.GPS
      { lat = gps.lat,
        lon = gps.lon
      }

instance FromBeckn Address.Address Address where
  fromBeckn addr =
    Address
      { door = addr.door,
        building = fromMaybe "" $ addr.building,
        street = addr.street,
        area = fromMaybe "" $ addr.locality,
        city = addr.city,
        country = addr.country,
        areaCode = addr.area_code,
        state = addr.state
      }

instance ToBeckn Address.Address Address where
  toBeckn addr =
    Address.Address
      { name = Nothing,
        door = addr.door,
        building = Just $ addr.building,
        street = addr.street,
        locality = Just $ addr.area,
        ward = Nothing,
        city = addr.city,
        state = addr.state,
        country = addr.country,
        area_code = addr.areaCode
      }

instance FromBeckn Location.Location Location where
  fromBeckn loc =
    Location
      { gps = fromBeckn <$> loc.gps,
        address = fromBeckn <$> loc.address,
        city = fromBeckn <$> loc.city
      }

instance ToBeckn Location.Location Location where
  toBeckn loc =
    Location.Location
      { gps = toBeckn <$> loc.gps,
        address = toBeckn <$> loc.address,
        station_code = Nothing,
        city = toBeckn <$> loc.city,
        country = Nothing,
        circle = Nothing,
        polygon = Nothing,
        _3dspace = Nothing
      }

instance FromBeckn Stop.Stop Stop where
  fromBeckn stop =
    Stop
      { location = fromBeckn $ stop.location,
        arrivalTime = StopTime (stop.arrival_time.est) (stop.arrival_time.act),
        departureTime = StopTime (stop.departure_time.est) (stop.departure_time.act)
      }

instance ToBeckn Stop.Stop Stop where
  toBeckn stop =
    Stop.Stop
      { id = "",
        descriptor = Nothing,
        location = toBeckn $ stop.location,
        arrival_time = Stop.StopTime (stop.arrivalTime.estimated) (stop.arrivalTime.actual),
        departure_time = Stop.StopTime (stop.departureTime.estimated) (stop.departureTime.actual),
        transfers = []
      }

instance FromBeckn DV.DecimalValue DecimalValue where
  fromBeckn value =
    DecimalValue
      { integral = value.integral,
        fractional = value.fractional
      }

instance ToBeckn DV.DecimalValue DecimalValue where
  toBeckn value =
    DV.DecimalValue
      { integral = value.integral,
        fractional = value.fractional
      }

instance FromBeckn Price.Price DecimalValue where
  fromBeckn price = DecimalValue (price ^. #value . _Just . #integral) (price ^. #value . _Just . #fractional)

instance ToBeckn Price.Price DecimalValue where
  toBeckn value =
    Price.Price
      { currency = "INR",
        value = Just $ toBeckn value,
        estimated_value = Nothing,
        computed_value = Nothing,
        listed_value = Nothing,
        offered_value = Nothing,
        minimum_value = Nothing,
        maximum_value = Nothing
      }

instance FromBeckn Driver.Driver Driver where
  fromBeckn driver =
    Driver
      { name = driver.name.given_name,
        gender = driver.gender,
        phones = driver.phones,
        rating = driver.rating,
        registeredAt = driver.registeredAt
      }

instance ToBeckn Driver.Driver Driver where
  toBeckn driver =
    Driver.Driver
      { name = Person.Name Nothing Nothing (driver.name) Nothing Nothing Nothing,
        image = Nothing,
        dob = Nothing,
        organization_name = Nothing,
        gender = driver.gender,
        email = Nothing,
        phones = driver.phones,
        experience = Nothing,
        rating = driver.rating,
        registeredAt = driver.registeredAt
      }

instance FromBeckn Traveller.Traveller Traveller where
  fromBeckn traveller =
    Traveller
      { name = traveller.name.given_name,
        gender = traveller.gender,
        phones = traveller.phones
      }

instance ToBeckn Traveller.Traveller Traveller where
  toBeckn traveller =
    Traveller.Traveller
      { name = Person.Name Nothing Nothing (traveller.name) Nothing Nothing Nothing,
        image = Nothing,
        dob = Nothing,
        organization_name = Nothing,
        gender = traveller.gender,
        email = Nothing,
        phones = traveller.phones,
        origin_stop_id = "",
        destination_stop_id = ""
      }

instance FromBeckn Payload.Payload [Traveller] where
  fromBeckn payload = fromBeckn <$> payload.travellers

instance ToBeckn Payload.Payload [Traveller] where
  toBeckn travellers =
    Payload.Payload
      { luggage = Nothing,
        traveller_count = Nothing,
        travellers = toBeckn <$> travellers,
        travel_group = Nothing
      }

instance FromBeckn Trip.Trip Trip where
  fromBeckn trip =
    let mbPrice = trip.fare
        mbFare = case mbPrice of
          Nothing -> Nothing
          Just p -> p.value
     in Trip
          { id = trip.id,
            pickup = fromBeckn <$> trip.pickup,
            drop = fromBeckn <$> trip.drop,
            vehicle = fromBeckn <$> trip.vehicle,
            driver = fromBeckn <$> trip.driver,
            travellers = [],
            fare = fromBeckn <$> mbFare
          }

instance ToBeckn Trip.Trip Trip where
  toBeckn trip =
    Trip.Trip
      { id = trip.id,
        pickup = toBeckn <$> trip.pickup,
        drop = toBeckn <$> trip.drop,
        state = Nothing,
        vehicle = toBeckn <$> trip.vehicle,
        driver = toBeckn <$> trip.driver,
        payload = toBeckn $ trip.travellers,
        fare = toBeckn <$> trip.fare,
        route = Nothing
      }

instance FromBeckn Tracking.Tracking Tracking where
  fromBeckn tracking = Tracking {url = tracking.url}

instance ToBeckn Tracking.Tracking Tracking where
  toBeckn tracking =
    Tracking.Tracking
      { url = tracking.url,
        required_params = Nothing,
        metadata = Nothing
      }

instance FromBeckn Category.Category Provider where
  fromBeckn category =
    Provider
      { id = category.id,
        name = category.descriptor.name,
        phones = Tag.value <$> filter (\x -> x.key == "contacts") (category.tags),
        info = readMaybe . T.unpack . Tag.value =<< find (\x -> x.key == "stats") (category.tags)
      }

instance ToBeckn Category.Category Provider where
  toBeckn category =
    Category.Category
      { id = category.id,
        descriptor =
          Descriptor.Descriptor
            { name = category.name,
              code = Nothing,
              symbol = Nothing,
              short_desc = Nothing,
              long_desc = Nothing,
              images = Nothing,
              audio = Nothing,
              _3d_render = Nothing
            },
        parent_category_id = Nothing,
        tags = []
      }
