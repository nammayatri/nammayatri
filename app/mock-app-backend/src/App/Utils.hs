module App.Utils where

import qualified Beckn.Types.API.Search as Search
import Beckn.Types.Core.Context
import Beckn.Types.Core.Location
import Beckn.Types.Core.ScalarRange
import Beckn.Types.Mobility.Intent
import qualified Beckn.Types.Mobility.Stop as Stop
import Beckn.Types.Mobility.Vehicle
import Data.Time
import EulerHS.Prelude

address :: Address
address =
  Address
    { door = "#817",
      building = "Juspay Apartments",
      street = "27th Main",
      area = "8th Block Koramangala",
      city = "Bangalore",
      country = "India",
      area_code = "560047"
    }

location :: Location
location =
  Location
    { _type = "address",
      _gps = Nothing,
      _address = Just address,
      _station_code = Nothing,
      _area_code = Nothing,
      _city = Nothing,
      _country = Nothing,
      _circle = Nothing,
      _polygon = Nothing,
      _3dspace = Nothing
    }

intentVehicle :: Vehicle
intentVehicle =
  Vehicle
    { category = Just "CAR",
      capacity = Nothing,
      make = Nothing,
      model = Nothing,
      size = Nothing,
      variant = "SUV",
      color = Nothing,
      energy_type = Nothing,
      registration = Nothing
    }

intentPayload :: Payload
intentPayload =
  Payload
    { _travellers = TravellerReqInfo {_count = 0},
      _luggage = Luggage {_count = 2, _weight_range = Nothing, _dimensions = Nothing}
    }

fareRange :: ScalarRange
fareRange = ScalarRange {_min = 10, _max = 1000}

buildIntent :: LocalTime -> Intent
buildIntent localTime =
  Intent
    { _query_string = Nothing,
      _provider_id = Nothing,
      _category_id = Nothing,
      _item_id = Nothing,
      _origin = getStop localTime,
      _destination = getStop localTime,
      _stops = [],
      _vehicle = intentVehicle,
      _payload = intentPayload,
      _transfer_attrs = Nothing,
      _fare_range = fareRange,
      _tags = []
    }

getStop :: LocalTime -> Stop.Stop
getStop stopTime =
  Stop.Stop
    { _descriptor = Nothing,
      _location = location,
      _arrival_time = Stop.StopTime stopTime (Just stopTime),
      _departure_time = Stop.StopTime stopTime (Just stopTime)
    }

buildContext :: Text -> Text -> LocalTime -> Context
buildContext act tid localTime =
  Context
    { _domain = "FINAL-MILE-DELIVERY",
      _action = act,
      _version = Just "0.8.0",
      _transaction_id = tid,
      _session_id = Nothing,
      _timestamp = localTime,
      _token = Nothing,
      _status = Nothing
    }

searchReq :: Text -> Text -> LocalTime -> Search.SearchReq
searchReq act tid localTime =
  Search.SearchReq
    { context = buildContext act tid localTime,
      message = Search.SearchIntent $ buildIntent localTime
    }

getFutureTime :: IO LocalTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  (zonedTimeToLocalTime . utcToZonedTime utc) . addUTCTime 7200 <$> getCurrentTime

buildSearchReq :: Text -> IO Search.SearchReq
buildSearchReq guid = searchReq "search" guid <$> getFutureTime
