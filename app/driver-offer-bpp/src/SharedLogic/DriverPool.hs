{-# LANGUAGE DerivingVia #-}

module SharedLogic.DriverPool
  ( calculateDriverPool,
  )
where

import Beckn.External.Maps.Google as GoogleMaps
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.List.NonEmpty as NE
import qualified Domain.Types.Organization as SOrg
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.Prelude hiding (id)
import GHC.Float (double2Int)
import qualified Storage.Queries.Person as QP
import Tools.Metrics

calculateDriverPool ::
  ( Esq.EsqDBReplicaFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleCfg r,
    HasPrettyLogger m r,
    HasCoordinates a
  ) =>
  Maybe Variant ->
  a ->
  Id SOrg.Organization ->
  Bool ->
  Bool ->
  m [GoogleMaps.GetDistanceResult QP.DriverPoolResult LatLong]
calculateDriverPool variant pickup orgId onlyNotOnRide shouldFilterByActualDistance = do
  radius <- fromIntegral <$> asks (.defaultRadiusOfSearch)
  mbDriverPositionInfoExpiry <- asks (.driverPositionInfoExpiry)
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      Esq.runInReplica $
        QP.getNearestDrivers
          variant
          pickupLatLong
          radius
          orgId
          onlyNotOnRide
          mbDriverPositionInfoExpiry
  logPretty DEBUG "approxDriverPool" approxDriverPool
  case approxDriverPool of
    [] -> pure []
    (a : pprox) ->
      if shouldFilterByActualDistance
        then filterOutDriversWithDistanceAboveThreshold radius pickupLatLong (a :| pprox)
        else return $ buildGetDistanceResult <$> approxDriverPool
  where
    pickupLatLong = getCoordinates pickup
    buildGetDistanceResult :: QP.DriverPoolResult -> GoogleMaps.GetDistanceResult QP.DriverPoolResult LatLong
    buildGetDistanceResult driverMetadata =
      let distance = driverMetadata.distanceToDriver
          duration = distance / 30000 * 3600 -- Average speed of 30km/hr
       in GoogleMaps.GetDistanceResult
            { origin = driverMetadata,
              destination = pickupLatLong,
              distance = Meters . double2Int $ distance,
              duration = Seconds . double2Int $ duration,
              status = "OK"
            }

filterOutDriversWithDistanceAboveThreshold ::
  ( CoreMetrics m,
    MonadFlow m,
    HasGoogleCfg r,
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
