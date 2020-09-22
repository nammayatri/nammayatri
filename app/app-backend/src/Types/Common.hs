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
import qualified Beckn.Types.Core.Tag as Tag
import qualified Beckn.Types.Core.Tracking as Tracking
import qualified Beckn.Types.Mobility.Driver as Driver
import qualified Beckn.Types.Mobility.Payload as Payload
import qualified Beckn.Types.Mobility.Stop as Stop
import qualified Beckn.Types.Mobility.Traveller as Traveller
import qualified Beckn.Types.Mobility.Trip as Trip
import qualified Beckn.Types.Mobility.Vehicle as Vehicle
import Control.Lens.Prism (_Just)
import Data.Time (UTCTime)
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
  { gps :: Maybe GPS,
    address :: Maybe Address,
    city :: Maybe City
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data StopTime = StopTime
  { estimated :: UTCTime,
    actual :: Maybe UTCTime
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
    info :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance FromBeckn Location.City City where
  fromBeckn city = City $ city ^. #name

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
      _ -> OTHER

instance ToBeckn Text VehicleCategory where
  toBeckn category =
    case category of
      CAR -> "CAR"
      MOTORCYCLE -> "MOTORCYCLE"
      BICYCLE -> "BICYCLE"
      OTHER -> "OTHER"
      TRUCK -> "TRUCK"

instance FromBeckn Vehicle.Vehicle Vehicle where
  fromBeckn vehicle =
    Vehicle
      { category = fromBeckn <$> vehicle ^. #category,
        capacity = vehicle ^. #capacity,
        model = vehicle ^. #model,
        variant = vehicle ^. #variant,
        registrationNumber = Vehicle.number <$> (vehicle ^. #registration)
      }

instance ToBeckn Vehicle.Vehicle Vehicle where
  toBeckn vehicle =
    Vehicle.Vehicle
      { category = toBeckn <$> vehicle ^. #category,
        capacity = vehicle ^. #capacity,
        make = Nothing,
        model = vehicle ^. #model,
        size = Nothing,
        variant = vehicle ^. #variant,
        color = Nothing,
        energy_type = Nothing,
        registration = Nothing
      }

instance FromBeckn Location.GPS GPS where
  fromBeckn gps =
    GPS
      { lat = gps ^. #lat,
        lon = gps ^. #lon
      }

instance ToBeckn Location.GPS GPS where
  toBeckn gps =
    Location.GPS
      { lat = gps ^. #lat,
        lon = gps ^. #lon
      }

instance FromBeckn Address.Address Address where
  fromBeckn addr =
    Address
      { door = addr ^. #_door,
        building = fromMaybe "" $ addr ^. #_building,
        street = addr ^. #_street,
        area = fromMaybe "" $ addr ^. #_locality,
        city = addr ^. #_city,
        country = addr ^. #_country,
        areaCode = addr ^. #_area_code
      }

instance ToBeckn Address.Address Address where
  toBeckn addr =
    Address.Address
      { _name = Nothing,
        _door = addr ^. #door,
        _building = Just $ addr ^. #building,
        _street = addr ^. #street,
        _locality = Just $ addr ^. #area,
        _ward = Nothing,
        _city = addr ^. #city,
        _state = "",
        _country = addr ^. #country,
        _area_code = addr ^. #areaCode
      }

instance FromBeckn Location.Location Location where
  fromBeckn loc =
    Location
      { gps = fromBeckn <$> loc ^. #_gps,
        address = fromBeckn <$> loc ^. #_address,
        city = fromBeckn <$> loc ^. #_city
      }

instance ToBeckn Location.Location Location where
  toBeckn loc =
    Location.Location
      { _gps = toBeckn <$> loc ^. #gps,
        _address = toBeckn <$> loc ^. #address,
        _station_code = Nothing,
        _city = toBeckn <$> loc ^. #city,
        _country = Nothing,
        _circle = Nothing,
        _polygon = Nothing,
        _3dspace = Nothing
      }

instance FromBeckn Stop.Stop Stop where
  fromBeckn stop =
    Stop
      { location = fromBeckn $ stop ^. #_location,
        arrivalTime = StopTime (stop ^. #_arrival_time . #_est) (stop ^. #_arrival_time . #_act),
        departureTime = StopTime (stop ^. #_departure_time . #_est) (stop ^. #_departure_time . #_act)
      }

instance ToBeckn Stop.Stop Stop where
  toBeckn stop =
    Stop.Stop
      { _id = "",
        _descriptor = Nothing,
        _location = toBeckn $ stop ^. #location,
        _arrival_time = Stop.StopTime (stop ^. #arrivalTime . #estimated) (stop ^. #arrivalTime . #actual),
        _departure_time = Stop.StopTime (stop ^. #departureTime . #estimated) (stop ^. #departureTime . #actual),
        _transfers = []
      }

instance FromBeckn DV.DecimalValue DecimalValue where
  fromBeckn value =
    DecimalValue
      { integral = value ^. #_integral,
        fractional = value ^. #_fractional
      }

instance ToBeckn DV.DecimalValue DecimalValue where
  toBeckn value =
    DV.DecimalValue
      { _integral = value ^. #integral,
        _fractional = value ^. #fractional
      }

instance FromBeckn Price.Price DecimalValue where
  fromBeckn price = DecimalValue (price ^. #_value . _Just . #_integral) (price ^. #_value . _Just . #_fractional)

instance ToBeckn Price.Price DecimalValue where
  toBeckn value =
    Price.Price
      { _currency = "INR",
        _value = Just $ toBeckn value,
        _estimated_value = Nothing,
        _computed_value = Nothing,
        _listed_value = Nothing,
        _offered_value = Nothing,
        _minimum_value = Nothing,
        _maximum_value = Nothing
      }

instance FromBeckn Driver.Driver Driver where
  fromBeckn driver =
    Driver
      { name = driver ^. #name . #_given_name,
        gender = driver ^. #gender,
        phones = driver ^. #phones
      }

instance ToBeckn Driver.Driver Driver where
  toBeckn driver =
    Driver.Driver
      { name = Person.Name Nothing Nothing (driver ^. #name) Nothing Nothing Nothing,
        image = Nothing,
        dob = Nothing,
        organization_name = Nothing,
        gender = driver ^. #gender,
        email = Nothing,
        phones = driver ^. #phones,
        experience = Nothing,
        rating = Nothing
      }

instance FromBeckn Traveller.Traveller Traveller where
  fromBeckn traveller =
    Traveller
      { name = traveller ^. #_name . #_given_name,
        gender = traveller ^. #_gender,
        phones = traveller ^. #_phones
      }

instance ToBeckn Traveller.Traveller Traveller where
  toBeckn traveller =
    Traveller.Traveller
      { _name = Person.Name Nothing Nothing (traveller ^. #name) Nothing Nothing Nothing,
        _image = Nothing,
        _dob = Nothing,
        _organization_name = Nothing,
        _gender = traveller ^. #gender,
        _email = Nothing,
        _phones = traveller ^. #phones,
        _origin_stop_id = "",
        _destination_stop_id = ""
      }

instance FromBeckn Payload.Payload [Traveller] where
  fromBeckn payload = fromBeckn <$> payload ^. #_travellers

instance ToBeckn Payload.Payload [Traveller] where
  toBeckn travellers =
    Payload.Payload
      { _luggage = Nothing,
        _traveller_count = Nothing,
        _travellers = toBeckn <$> travellers,
        _travel_group = Nothing
      }

instance FromBeckn Trip.Trip Trip where
  fromBeckn trip =
    let mbPrice = trip ^. #fare
        mbFare = case mbPrice of
          Nothing -> Nothing
          Just p -> p ^. #_value
     in Trip
          { id = trip ^. #id,
            pickup = fromBeckn <$> trip ^. #pickup,
            drop = fromBeckn <$> trip ^. #drop,
            vehicle = fromBeckn <$> trip ^. #vehicle,
            driver = fromBeckn <$> trip ^. #driver,
            travellers = [],
            fare = fromBeckn <$> mbFare
          }

instance ToBeckn Trip.Trip Trip where
  toBeckn trip =
    Trip.Trip
      { id = trip ^. #id,
        pickup = toBeckn <$> trip ^. #pickup,
        drop = toBeckn <$> trip ^. #drop,
        state = Nothing,
        vehicle = toBeckn <$> trip ^. #vehicle,
        driver = toBeckn <$> trip ^. #driver,
        payload = toBeckn $ trip ^. #travellers,
        fare = toBeckn <$> trip ^. #fare,
        route = Nothing
      }

instance FromBeckn Tracking.Tracking Tracking where
  fromBeckn tracking = Tracking {url = tracking ^. #_url}

instance ToBeckn Tracking.Tracking Tracking where
  toBeckn tracking =
    Tracking.Tracking
      { _url = tracking ^. #url,
        _required_params = Nothing,
        _metadata = Nothing
      }

instance FromBeckn Category.Category Provider where
  fromBeckn category =
    Provider
      { id = category ^. #_id,
        name = category ^. #_descriptor . #_name,
        phones = Tag._value <$> filter (\x -> x ^. #_key == "contacts") (category ^. #_tags),
        info = Tag._value <$> find (\x -> x ^. #_key == "stats") (category ^. #_tags)
      }

instance ToBeckn Category.Category Provider where
  toBeckn category =
    Category.Category
      { _id = category ^. #id,
        _descriptor =
          Descriptor.Descriptor
            { _name = category ^. #name,
              _code = Nothing,
              _symbol = Nothing,
              _short_desc = Nothing,
              _long_desc = Nothing,
              _images = Nothing,
              _audio = Nothing,
              _3d_render = Nothing
            },
        _parent_category_id = Nothing,
        _tags = []
      }
