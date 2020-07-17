module App.Utils where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Location
import Beckn.Types.FMD.Intent
import Data.Time
import EulerHS.Prelude
import "beckn-gateway" Types.API.Search

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

buildIntent :: Intent
buildIntent =
  Intent
    { _query_string = Nothing,
      _provider_id = Nothing,
      _category_id = Nothing,
      _item_id = Nothing,
      _pickups = [location],
      _drops = [location],
      _packages = [],
      _tags = []
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

searchReq :: Text -> Text -> LocalTime -> SearchReq
searchReq act tid localTime =
  SearchReq
    { context = buildContext act tid localTime,
      message = toJSON buildIntent
    }

getFutureTime :: IO LocalTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  (zonedTimeToLocalTime . utcToZonedTime utc) . addUTCTime 7200 <$> getCurrentTime

buildSearchReq :: Text -> IO SearchReq
buildSearchReq guid = searchReq "search" guid <$> getFutureTime
