module SharedLogic.DriverPool
  ( calculateDriverPool,
  )
where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Storage.Esqueleto (Transactionable)
import Beckn.Types.Id
import Beckn.Types.MapSearch
import qualified Beckn.Types.MapSearch as GoogleMaps
import qualified Domain.Types.Organization as SOrg
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import Tools.Metrics
import Utils.Common

calculateDriverPool ::
  ( Transactionable m,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  LatLong ->
  Id SOrg.Organization ->
  m [(QP.DriverPoolResult, GoogleMaps.GetDistanceResultInfo)]
calculateDriverPool pickupLatLong orgId = do
  radius <- fromIntegral <$> asks (.defaultRadiusOfSearch)
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QP.getNearestDrivers
        pickupLatLong
        radius
        orgId
  case approxDriverPool of
    [] -> pure []
    (a : pprox) -> filterOutDriversWithDistanceAboveThreshold radius pickupLatLong (a :| pprox)

filterOutDriversWithDistanceAboveThreshold ::
  ( Transactionable m,
    CoreMetrics m,
    MonadFlow m,
    HasGoogleMaps m r
  ) =>
  Integer ->
  LatLong ->
  NonEmpty QP.DriverPoolResult ->
  m [(QP.DriverPoolResult, GoogleMaps.GetDistanceResultInfo)]
filterOutDriversWithDistanceAboveThreshold threshold pickupLatLong driverPoolResults = do
  getDistanceResults <- GoogleMaps.getDistancesGeneral (Just GoogleMaps.CAR) driverPoolResults (pickupLatLong :| []) zipFunc Nothing
  pure $ filter (filterFunc . snd) getDistanceResults
  where
    zipFunc dpRes _ estDist = (dpRes, estDist)
    filterFunc estDist = getDistanceInMeter estDist.distance <= fromIntegral threshold
