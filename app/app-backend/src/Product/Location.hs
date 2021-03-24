module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Types.API.Location as Location
import Types.Error

getRoute :: Person.Person -> Location.Request -> FlowHandler Location.Response
getRoute _person Location.Request {..} =
  withFlowHandler $
    MapSearch.getRoute getRouteRequest
      >>= either
        (throwErrorWithInfo UnableToGetRoute . show)
        return
  where
    getRouteRequest = do
      let mapToMapPoint (Location.LatLong lat long) = MapSearch.LatLong $ MapSearch.PointXY lat long
      MapSearch.Request
        { waypoints = mapToMapPoint <$> waypoints,
          mode = mode <|> Just MapSearch.CAR,
          departureTime = Nothing,
          arrivalTime = Nothing,
          calcPoints
        }
