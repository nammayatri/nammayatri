{-# LANGUAGE DerivingVia #-}

module SharedLogic.DriverPool
  ( calculateDriverPool,
    randomizeAndLimitSelection,
    intelligentPoolSelection,
    incrementAcceptanceCount,
    incrementTotalCount,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import qualified Beckn.Types.SlidingWindowCounters as SWC
import Beckn.Utils.Common
import qualified Beckn.Utils.SlidingWindowCounters as SWC
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Domain.Types.Merchant as DM
import Domain.Types.Vehicle.Variant (Variant)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import GHC.Float (double2Int)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person as QP
import System.Random
import Tools.Maps as Maps
import Tools.Metrics

mkAcceptanceKey :: Text -> Text
mkAcceptanceKey = (<> "-quote-accepted")

withWindowOptions ::
  ( Redis.HedisFlow m r,
    HasField "acceptanceWindowOptions" r SWC.SlidingWindowOptions,
    L.MonadFlow m
  ) =>
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withWindowOptions fn = do
  asks (.acceptanceWindowOptions) >>= fn

incrementTotalCount ::
  ( Redis.HedisFlow m r,
    HasField "acceptanceWindowOptions" r SWC.SlidingWindowOptions,
    L.MonadFlow m
  ) =>
  Id a ->
  m ()
incrementTotalCount driverId = withWindowOptions $ SWC.incrementTotalCount (getId driverId)

incrementAcceptanceCount ::
  ( Redis.HedisFlow m r,
    HasField "acceptanceWindowOptions" r SWC.SlidingWindowOptions,
    L.MonadFlow m
  ) =>
  Id a ->
  m ()
incrementAcceptanceCount driverId = withWindowOptions $ SWC.incrementWindowCount (mkAcceptanceKey $ getId driverId)

intelligentPoolSelection ::
  ( Redis.HedisFlow m r,
    HasField "acceptanceWindowOptions" r SWC.SlidingWindowOptions,
    L.MonadFlow m
  ) =>
  [Maps.GetDistanceResp QP.DriverPoolResult LatLong] ->
  Int ->
  m [Maps.GetDistanceResp QP.DriverPoolResult LatLong]
intelligentPoolSelection dp n =
  withWindowOptions $
    \wo ->
      map snd
        . take n
        . sortOn (Down . fst)
        <$> ( (\poolWithRatio -> logInfo ("Drivers in Pool with acceptance ratios " <> show poolWithRatio) $> poolWithRatio)
                =<< mapM (\dPoolRes -> (,dPoolRes) <$> SWC.getLatestRatio (getId dPoolRes.origin.driverId) mkAcceptanceKey wo) dp
            )

randomizeAndLimitSelection ::
  (L.MonadFlow m) =>
  [Maps.GetDistanceResp QP.DriverPoolResult LatLong] ->
  Int ->
  m [Maps.GetDistanceResp QP.DriverPoolResult LatLong]
randomizeAndLimitSelection driverPool limit = do
  let poolLen = length driverPool
      startIdx = 0
      endIdx = poolLen - 1
  randomNumList <- getRandomNumberList startIdx endIdx limit
  return $ fmap (driverPool !!) randomNumList

getRandomNumberList :: (L.MonadFlow m) => Int -> Int -> Int -> m [Int]
getRandomNumberList start end count = do
  n <- round <$> L.runIO getPOSIXTime
  let pureGen = mkStdGen n
  return $ toList $ nextNumber pureGen Set.empty
  where
    nextNumber :: RandomGen g => g -> Set.Set Int -> Set.Set Int
    nextNumber gen acc =
      if Set.size acc == min (end - start + 1) count
        then acc
        else
          let (n, gen') = randomR (start, end) gen
           in nextNumber gen' (Set.union (Set.singleton n) acc)

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
