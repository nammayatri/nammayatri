module Mobility.Utils where

import Beckn.Types.MapSearch
import qualified Data.List.NonEmpty as NE
import EulerHS.Prelude
import Mobility.Fixtures.Routes
import "app-backend" Types.API.Search

searchReqFromUpdatesList :: LocationUpdates -> (LatLong, LatLong, SearchReq)
searchReqFromUpdatesList updList =
  let origin = NE.head $ NE.head updList
      destination = NE.last $ NE.last updList
      req =
        OneWaySearch $
          OneWaySearchReq
            { origin = SearchReqLocation $ NE.head $ NE.head updList,
              destination = SearchReqLocation $ NE.last $ NE.last updList
            }
   in (origin, destination, req)
