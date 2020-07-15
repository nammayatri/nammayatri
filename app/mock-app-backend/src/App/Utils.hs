module App.Utils where

import qualified Beckn.Types.API.Search as Search
import Beckn.Types.Core.Context
import Beckn.Types.Core.Location
import Beckn.Types.Core.ScalarRange
import Beckn.Types.Mobility.Intent
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
    { travellers = TravellerReqInfo {count = 0},
      luggage = Luggage {count = 2, weight_range = Nothing, dimensions = Nothing}
    }

fareRange :: ScalarRange
fareRange = ScalarRange {_min = 10, _max = 1000, _unit = "Rs"}

buildIntent :: LocalTime -> Intent
buildIntent localTime =
  Intent
    { domain = "FINAL-MILE-DELIVERY",
      origin = location,
      destination = location,
      time = localTime,
      stops = [],
      vehicle = intentVehicle,
      providers = [],
      payload = intentPayload,
      transfer_attrs = Nothing,
      fare_range = fareRange,
      tags = []
    }

buildContext :: Text -> Text -> LocalTime -> Context
buildContext act tid localTime =
  Context
    { domain = "FINAL-MILE-DELIVERY",
      action = act,
      version = Nothing,
      transaction_id = tid,
      message_id = Nothing,
      timestamp = localTime,
      dummy = "dummy"
    }

searchReq :: Text -> Text -> LocalTime -> Search.SearchReq
searchReq act tid localTime =
  Search.SearchReq
    { context = buildContext act tid localTime,
      message = buildIntent localTime
    }

getFutureTime :: IO LocalTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  (zonedTimeToLocalTime . utcToZonedTime utc) . addUTCTime 7200 <$> getCurrentTime

buildSearchReq :: Text -> IO Search.SearchReq
buildSearchReq guid = searchReq "search" guid <$> getFutureTime
