{-# LANGUAGE DerivingVia #-}

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
import Beckn.Utils.Common
import Data.List.NonEmpty as NE
import qualified Domain.Types.Organization as SOrg
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import Tools.Metrics

calculateDriverPool ::
  ( Transactionable m,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleMaps m r,
    HasPrettyLogger m r
  ) =>
  Maybe Variant ->
  LatLong ->
  Id SOrg.Organization ->
  Bool ->
  m [GoogleMaps.GetDistanceResult QP.DriverPoolResult LatLong]
calculateDriverPool variant pickupLatLong orgId onlyNotOnRide = do
  radius <- fromIntegral <$> asks (.defaultRadiusOfSearch)
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QP.getNearestDrivers
        variant
        pickupLatLong
        radius
        orgId
        onlyNotOnRide
  logPretty DEBUG "approxDriverPool" approxDriverPool
  case approxDriverPool of
    [] -> pure []
    (a : pprox) -> filterOutDriversWithDistanceAboveThreshold radius pickupLatLong (a :| pprox)

filterOutDriversWithDistanceAboveThreshold ::
  ( Transactionable m,
    CoreMetrics m,
    MonadFlow m,
    HasGoogleMaps m r,
    HasPrettyLogger m r
  ) =>
  Integer ->
  LatLong ->
  NonEmpty QP.DriverPoolResult ->
  m [GoogleMaps.GetDistanceResult QP.DriverPoolResult LatLong]
filterOutDriversWithDistanceAboveThreshold threshold pickupLatLong driverPoolResults = do
  getDistanceResults <- GoogleMaps.getDistances (Just GoogleMaps.CAR) driverPoolResults (pickupLatLong :| []) 
  logDebug $ "get distance results" <> show getDistanceResults
  let result = NE.filter filterFunc getDistanceResults
  logDebug $ "secondly filtered driver pool" <> show result
  pure result
  where
    filterFunc estDist = getMeters estDist.distance <= fromIntegral threshold
