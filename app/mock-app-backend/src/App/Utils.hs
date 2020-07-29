module App.Utils where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Location
import Beckn.Types.FMD.Intent
import Data.Time
import EulerHS.Prelude
import System.Environment (lookupEnv)
import "beckn-gateway" Types.API.Search

newtype FMDSearch = FMDSearch {intent :: Intent}
  deriving (Generic, Show, ToJSON, FromJSON)

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

gps :: GPS
gps =
  GPS
    { lat = "12.9401108",
      lon = "77.6206631"
    }

location :: Location
location =
  Location
    { _type = "gps",
      _gps = Just gps,
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

buildContext :: Text -> Text -> IO Context
buildContext act tid = do
  localTime <- getFutureTime
  bapNwAddr <- lookupEnv "MOCK_APP_NW_ADDRESS"
  return $
    Context
      { _domain = "FINAL-MILE-DELIVERY",
        _action = act,
        _country = Nothing,
        _city = Nothing,
        _core_version = Just "0.8.0",
        _domain_version = Just "0.7.0",
        _bap_nw_address = fromString <$> bapNwAddr,
        _bg_nw_address = Nothing,
        _bpp_nw_address = Nothing,
        _request_transaction_id = tid,
        _timestamp = localTime,
        _token = Nothing
      }

searchReq :: Text -> Text -> IO SearchReq
searchReq act tid =
  SearchReq <$> buildContext act tid <*> pure (toJSON $ FMDSearch buildIntent)

getFutureTime :: IO LocalTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  (zonedTimeToLocalTime . utcToZonedTime utc) . addUTCTime 7200 <$> getCurrentTime

buildSearchReq :: Text -> IO SearchReq
buildSearchReq = searchReq "search"
