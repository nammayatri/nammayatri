module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Types.API.Location as Location
import qualified Types.Common as Common
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Person as Person
import Utils.Common

getRoute :: Id Person.Person -> Location.Request -> FlowHandler Location.Response
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . MapSearch.getRoutes

getDistance ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  Common.Location ->
  Common.Location ->
  m (Maybe Double)
getDistance pickupLoc dropLoc = do
  pickupMapPoint <-
    Common.gps pickupLoc & fromMaybeM (InvalidRequest "No long / lat.")
      >>= mkMapPoint
  dropMapPoint <-
    Common.gps dropLoc & fromMaybeM (InvalidRequest "No long / lat.")
      >>= mkMapPoint
  MapSearch.getDistanceMb (Just MapSearch.CAR) [pickupMapPoint, dropMapPoint]

mkMapPoint :: (MonadThrow m, Log m) => Common.GPS -> m MapSearch.LatLong
mkMapPoint (Common.GPS lat lon) = do
  MapSearch.LatLong
    <$> readLatLng lat
    <*> readLatLng lon

readLatLng :: (MonadThrow m, Log m) => Text -> m Double
readLatLng text = do
  readMaybe (T.unpack text) & fromMaybeM (InternalError "Location read error")
