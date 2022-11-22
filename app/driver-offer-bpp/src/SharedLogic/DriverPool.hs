{-# LANGUAGE DerivingVia #-}

module SharedLogic.DriverPool
  ( calculateDriverPool,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.List.NonEmpty as NE
import qualified Domain.Types.Merchant as DM
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.Prelude hiding (id)
import GHC.Float (double2Int)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person as QP
import Tools.Maps as Maps
import Tools.Metrics

calculateDriverPool ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasPrettyLogger m r,
    HasCoordinates a
  ) =>
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Bool ->
  Bool ->
  m [Maps.GetDistanceResp QP.DriverPoolResult LatLong]
calculateDriverPool variant pickup merchantId onlyNotOnRide shouldFilterByActualDistance = do
  radius <- fromIntegral <$> asks (.defaultRadiusOfSearch)
  mbDriverPositionInfoExpiry <- asks (.driverPositionInfoExpiry)
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      Esq.runInReplica $
        QP.getNearestDrivers
          variant
          pickupLatLong
          radius
          merchantId
          onlyNotOnRide
          mbDriverPositionInfoExpiry
  logPretty DEBUG "approxDriverPool" approxDriverPool
  case approxDriverPool of
    [] -> pure []
    (a : pprox) ->
      if shouldFilterByActualDistance
        then filterOutDriversWithDistanceAboveThreshold merchantId radius pickupLatLong (a :| pprox)
        else return $ buildGetDistanceResult <$> approxDriverPool
  where
    pickupLatLong = getCoordinates pickup
    buildGetDistanceResult :: QP.DriverPoolResult -> Maps.GetDistanceResp QP.DriverPoolResult LatLong
    buildGetDistanceResult driverMetadata =
      let distance = driverMetadata.distanceToDriver
          duration = distance / 30000 * 3600 -- Average speed of 30km/hr
       in Maps.GetDistanceResp
            { origin = driverMetadata,
              destination = pickupLatLong,
              distance = Meters . double2Int $ distance,
              duration = Seconds . double2Int $ duration,
              status = "OK"
            }

filterOutDriversWithDistanceAboveThreshold ::
  ( CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasPrettyLogger m r
  ) =>
  Id DM.Merchant ->
  Integer ->
  LatLong ->
  NonEmpty QP.DriverPoolResult ->
  m [Maps.GetDistanceResp QP.DriverPoolResult LatLong]
filterOutDriversWithDistanceAboveThreshold orgId threshold pickupLatLong driverPoolResults = do
  getDistanceResults <-
    Maps.getDistances orgId $
      Maps.GetDistancesReq
        { origins = driverPoolResults,
          destinations = pickupLatLong :| [],
          travelMode = Just Maps.CAR
        }
  logDebug $ "get distance results" <> show getDistanceResults
  let result = NE.filter filterFunc getDistanceResults
  logDebug $ "secondly filtered driver pool" <> show result
  pure result
  where
    filterFunc estDist = getMeters estDist.distance <= fromIntegral threshold
