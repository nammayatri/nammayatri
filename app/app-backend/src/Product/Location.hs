module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Domain.Types.Person as Person
import EulerHS.Prelude
import qualified Types.API.Location as Location
import Types.Metrics (CoreMetrics)
import Utils.Common

getRoute :: Id Person.Person -> Location.Request -> FlowHandler Location.Response
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . MapSearch.getRoutes

getDistance ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  MapSearch.LatLong ->
  MapSearch.LatLong ->
  m (Maybe Double)
getDistance pickupLoc dropLoc = do
  MapSearch.getDistanceMb (Just MapSearch.CAR) [pickupLoc, dropLoc]
