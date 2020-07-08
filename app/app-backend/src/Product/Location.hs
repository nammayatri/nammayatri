module Product.Location where

import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Types.App
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Types.API.Location as Location
import Utils.Common (verifyToken)

getRoute :: Person.Person -> Location.Request -> FlowHandler Location.Response
getRoute _person Location.Request {..} = withFlowHandler $ do
  MapSearch.getRoute getRouteRequest
    >>= either
      (\err -> L.throwException $ err400 {errBody = show err})
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
