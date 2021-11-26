module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import EulerHS.Prelude
import qualified Types.API.Location as Location
import qualified Types.Common as Common
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Person as Person
import Utils.Common

getRoute :: Id Person.Person -> Location.Request -> FlowHandler Location.Response
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . MapSearch.getRoutes

getDistance ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  Common.GPS ->
  Common.GPS ->
  m (Maybe Double)
getDistance pickupLoc dropLoc = do
  let pickupMapPoint = mkMapPoint pickupLoc
      dropMapPoint = mkMapPoint dropLoc
  MapSearch.getDistanceMb (Just MapSearch.CAR) [pickupMapPoint, dropMapPoint]

mkMapPoint :: Common.GPS -> MapSearch.LatLong
mkMapPoint (Common.GPS lat lon) = MapSearch.LatLong lat lon
