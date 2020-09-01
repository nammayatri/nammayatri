{-# LANGUAGE OverloadedLabels #-}

module Types.Common where

import qualified Beckn.Types.Core.Address as Address
import qualified Beckn.Types.Core.DecimalValue as DV
import qualified Beckn.Types.Core.Descriptor as Descriptor
import qualified Beckn.Types.Core.Location as Location
import qualified Beckn.Types.Core.Person as Person
import qualified Beckn.Types.Core.Price as Price
import qualified Beckn.Types.Core.Provider as Provider
import qualified Beckn.Types.Core.Tracking as Tracking
import qualified Beckn.Types.Mobility.Driver as Driver
import qualified Beckn.Types.Mobility.Payload as Payload
import qualified Beckn.Types.Mobility.Stop as Stop
import qualified Beckn.Types.Mobility.Traveller as Traveller
import qualified Beckn.Types.Mobility.Trip as Trip
import qualified Beckn.Types.Mobility.Vehicle as Vehicle
import qualified Beckn.Types.Storage.ProductInstance as PI
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

-- This type class is not strictly an isomorphism. We use the name 'Iso' to
-- denote that it is always expected that a type from the beckn spec should
-- have a corresponding type defined by us allowing conversion (which may be
-- lossless) between the two, when defined as an instance of this typeclass.
class BecknSpecIso a b where
  fromBeckn :: a -> b
  toBeckn :: b -> a

instance BecknSpecIso Location.City City where
  fromBeckn city = City $ city ^. #name
  toBeckn (City cityName) =
    Location.City
      { name = cityName,
        code = ""
      }

instance BecknSpecIso Text VehicleCategory where
  fromBeckn category =
    case category of
      "CAR" -> CAR
      "MOTORCYCLE" -> MOTORCYCLE
      "BICYCLE" -> BICYCLE
      "OTHER" -> OTHER
      "TRUCK" -> TRUCK
      _ -> OTHER
  toBeckn category =
    case category of
      CAR -> "CAR"
      MOTORCYCLE -> "MOTORCYCLE"
      BICYCLE -> "BICYCLE"
      OTHER -> "OTHER"
      TRUCK -> "TRUCK"

instance BecknSpecIso Vehicle.Vehicle Vehicle where
  fromBeckn vehicle =
    Vehicle
      { category = fromBeckn <$> vehicle ^. #category,
        capacity = vehicle ^. #capacity,
        model = vehicle ^. #model,
        variant = vehicle ^. #variant,
        registrationNumber = Vehicle.number <$> (vehicle ^. #registration)
      }
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

instance BecknSpecIso Location.GPS GPS where
  fromBeckn gps =
    GPS
      { lat = gps ^. #lat,
        lon = gps ^. #lon
      }
  toBeckn gps =
    Location.GPS
      { lat = gps ^. #lat,
        lon = gps ^. #lon
      }

instance BecknSpecIso Address.Address Address where
  fromBeckn addr =
    Address
      { door = fromMaybe "" $ addr ^. #_door,
        building = fromMaybe "" $ addr ^. #_building,
        street = fromMaybe "" $ addr ^. #_street,
        area = fromMaybe "" $ addr ^. #_locality,
        city = addr ^. #_city,
        country = addr ^. #_country,
        areaCode = addr ^. #_area_code
      }
  toBeckn addr =
    Address.Address
      { _name = "",
        _door = Just $ addr ^. #door,
        _building = Just $ addr ^. #building,
        _street = Just $ addr ^. #street,
        _locality = Just $ addr ^. #area,
        _ward = Nothing,
        _city = addr ^. #city,
        _state = "",
        _country = addr ^. #country,
        _area_code = addr ^. #areaCode
      }

instance BecknSpecIso Location.Location Location where
  fromBeckn loc =
    Location
      { gps = fromBeckn <$> loc ^. #_gps,
        address = fromBeckn <$> loc ^. #_address,
        city = fromBeckn <$> loc ^. #_city
      }
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

instance BecknSpecIso Stop.Stop Stop where
  fromBeckn stop =
    Stop
      { location = fromBeckn $ stop ^. #_location,
        arrivalTime = StopTime (stop ^. #_arrival_time . #_est) (stop ^. #_arrival_time . #_act),
        departureTime = StopTime (stop ^. #_departure_time . #_est) (stop ^. #_departure_time . #_act)
      }
  toBeckn stop =
    Stop.Stop
      { _id = "",
        _descriptor = Nothing,
        _location = toBeckn $ stop ^. #location,
        _arrival_time = Stop.StopTime (stop ^. #arrivalTime . #estimated) (stop ^. #arrivalTime . #actual),
        _departure_time = Stop.StopTime (stop ^. #departureTime . #estimated) (stop ^. #departureTime . #actual),
        _transfers = []
      }

instance BecknSpecIso DV.DecimalValue DecimalValue where
  fromBeckn value =
    DecimalValue
      { integral = value ^. #_integral,
        fractional = value ^. #_fractional
      }
  toBeckn value =
    DV.DecimalValue
      { _integral = value ^. #integral,
        _fractional = value ^. #fractional
      }

instance BecknSpecIso Price.Price DecimalValue where
  fromBeckn price = DecimalValue (price ^. #_value . _Just . #_integral) (price ^. #_value . _Just . #_fractional)
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

instance BecknSpecIso Driver.Driver Driver where
  fromBeckn driver =
    Driver
      { name = driver ^. #name . #_given_name,
        gender = driver ^. #gender,
        phones = driver ^. #phones
      }
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

instance BecknSpecIso Traveller.Traveller Traveller where
  fromBeckn traveller =
    Traveller
      { name = traveller ^. #_name . #_given_name,
        gender = traveller ^. #_gender,
        phones = traveller ^. #_phones
      }
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

instance BecknSpecIso Payload.Payload [Traveller] where
  fromBeckn payload = fromBeckn <$> payload ^. #_travellers
  toBeckn travellers =
    Payload.Payload
      { _luggage = Nothing,
        _traveller_count = Nothing,
        _travellers = toBeckn <$> travellers,
        _travel_group = Nothing
      }

instance BecknSpecIso Trip.Trip Trip where
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

instance BecknSpecIso Tracking.Tracking Tracking where
  fromBeckn tracking = Tracking {url = tracking ^. #_url}
  toBeckn tracking =
    Tracking.Tracking
      { _url = tracking ^. #url,
        _required_params = Nothing,
        _metadata = Nothing
      }

instance BecknSpecIso Provider.Provider Provider where
  fromBeckn provider =
    Provider
      { id = provider ^. #_id,
        name = provider ^. #_descriptor . #_name,
        phones = provider ^. #_poc . _Just . #phones,
        info = provider ^. #_descriptor . #_long_desc
      }
  toBeckn provider =
    Provider.Provider
      { _id = provider ^. #id,
        _descriptor =
          Descriptor.Descriptor
            { _name = provider ^. #name,
              _code = Nothing,
              _symbol = Nothing,
              _short_desc = Nothing,
              _long_desc = provider ^. #info,
              _images = [],
              _audio = Nothing,
              _3d_render = Nothing
            },
        _poc =
          Just $
            Person.Person
              { name =
                  Person.Name
                    { _additional_name = Nothing,
                      _family_name = Nothing,
                      _given_name = "",
                      _call_sign = Nothing,
                      _honorific_prefix = Nothing,
                      _honorific_suffix = Nothing
                    },
                image = Nothing,
                dob = Nothing,
                organization_name = provider ^. #name,
                gender = "",
                email = Nothing,
                phones = provider ^. #phones
              }
      }

instance BecknSpecIso Text PI.ProductInstanceStatus where
  fromBeckn piStatus =
    case piStatus of
      "VALID" -> PI.VALID
      "INPROGRESS" -> PI.INPROGRESS
      "CONFIRMED" -> PI.CONFIRMED
      "COMPLETED" -> PI.COMPLETED
      "INSTOCK" -> PI.INSTOCK
      "OUTOFSTOCK" -> PI.OUTOFSTOCK
      "CANCELLED" -> PI.CANCELLED
      "EXPIRED" -> PI.EXPIRED
      _ -> PI.INVALID
  toBeckn piStatus =
    case piStatus of
      PI.VALID -> "VALID"
      PI.INVALID -> "INVALID"
      PI.INPROGRESS -> "INPROGRESS"
      PI.CONFIRMED -> "CONFIRMED"
      PI.COMPLETED -> "COMPLETED"
      PI.INSTOCK -> "INSTOCK"
      PI.OUTOFSTOCK -> "OUTOFSTOCK"
      PI.CANCELLED -> "CANCELLED"
      PI.EXPIRED -> "EXPIRED"
