module Beckn.External.Geo.API where

import Beckn.External.Geo.Types
import Beckn.Types.App
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant
import Servant.Client

type GeoAPI =
  "maps"
    :> "api"
    :> "geocode"
    :> "json"
    :> MandatoryQueryParam "latlng" Text
    :> MandatoryQueryParam "key" Text
    :> Get '[JSON] ReverseGeoResp

geoAPI :: Proxy GeoAPI
geoAPI = Proxy

getLocation lat lng key =
  ET.client geoAPI (lat <> "," <> lng) key
