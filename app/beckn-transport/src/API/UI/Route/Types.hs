module API.UI.Route.Types where

import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.MapSearch as MapSearch

type RouteRequest = MapSearch.Request

type RouteResponse = GoogleMaps.DirectionsResp
