{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module SharedLogic.DriverPool
  ( calculateDriverPool,
    calculateDriverPoolWithActualDist,
    calculateGoHomeDriverPool,
    calculateDriverCurrentlyOnRideWithActualDist,
    calculateDriverPoolCurrentlyOnRide,
    incrementTotalQuotesCount,
    incrementQuoteAcceptedCount,
    decrementTotalQuotesCount,
    getTotalQuotesSent,
    getLatestAcceptanceRatio,
    incrementTotalRidesCount,
    isThresholdRidesCompleted,
    incrementCancellationCount,
    getLatestCancellationRatio,
    getCurrentWindowAvailability,
    getQuotesCount,
    getPopupDelay,
    getValidSearchRequestCount,
    removeSearchReqIdFromMap,
    updateDriverSpeedInRedis,
    getDriverAverageSpeed,
    mkAvailableTimeKey,
    mkBlockListedDriversKey,
    PoolCalculationStage (..),
    module Reexport,
  )
where

import Control.Monad.Extra (mapMaybeM)
import Data.Fixed
import Data.List (partition)
import Data.List.Extra (notNull)
import qualified Data.List.NonEmpty as NE
import Data.Tuple.Extra (snd3)
import Domain.Action.UI.Route as DRoute
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.DriverIntelligentPoolConfig (IntelligentScores (IntelligentScores))
import qualified Domain.Types.Merchant.DriverIntelligentPoolConfig as DIPC
import Domain.Types.Merchant.DriverPoolConfig
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.SearchRequest
import Domain.Types.SearchTry
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude (head)
import qualified Kernel.Randomizer as Rnd
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import SharedLogic.DriverPool.Config as Reexport
import SharedLogic.DriverPool.Types as Reexport
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant.DriverIntelligentPoolConfig as DIP
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CTC
import qualified Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest as QDGR
import Storage.Queries.Person
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person.GetNearestDrivers as QPG
import Tools.Maps as Maps
import Tools.Metrics

mkTotalQuotesKey :: Text -> Text
mkTotalQuotesKey driverId = "driver-offer:DriverPool:Total-quotes:DriverId-" <> driverId

mkQuotesAcceptedKey :: Text -> Text
mkQuotesAcceptedKey driverId = "driver-offer:DriverPool:Quote-accepted:DriverId-" <> driverId

mkTotalRidesKey :: Text -> Text
mkTotalRidesKey driverId = "driver-offer:DriverPool:Total-Rides:DriverId-" <> driverId

mkRideCancelledKey :: Text -> Text
mkRideCancelledKey driverId = "driver-offer:DriverPool:Ride-cancelled:DriverId-" <> driverId

mkOldRatioKey :: Text -> Text -> Text
mkOldRatioKey driverId ratioType = "driver-offer:DriverPool:" <> ratioType <> ":DriverId-" <> driverId

mkAvailableTimeKey :: Text -> Text
mkAvailableTimeKey driverId = "driver-offer:DriverPool:Available-Time:DriverId-" <> driverId

windowFromIntelligentPoolConfig :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> (DIPC.DriverIntelligentPoolConfig -> SWC.SlidingWindowOptions) -> m SWC.SlidingWindowOptions
windowFromIntelligentPoolConfig merchantOpCityId windowKey = maybe defaultWindow windowKey <$> DIP.findByMerchantOpCityId merchantOpCityId
  where
    defaultWindow = SWC.SlidingWindowOptions 7 SWC.Days

withAcceptanceRatioWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withAcceptanceRatioWindowOption merchantOpCityId fn = windowFromIntelligentPoolConfig merchantOpCityId (.acceptanceRatioWindowOption) >>= fn

withCancellationRatioWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withCancellationRatioWindowOption merchantOpCityId fn = windowFromIntelligentPoolConfig merchantOpCityId (.cancellationRatioWindowOption) >>= fn

withAvailabilityTimeWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withAvailabilityTimeWindowOption merchantOpCityId fn = windowFromIntelligentPoolConfig merchantOpCityId (.availabilityTimeWindowOption) >>= fn

withMinQuotesToQualifyIntelligentPoolWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withMinQuotesToQualifyIntelligentPoolWindowOption merchantOpCityId fn = windowFromIntelligentPoolConfig merchantOpCityId (.minQuotesToQualifyForIntelligentPoolWindowOption) >>= fn

decrementTotalQuotesCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  Id SearchTry ->
  m ()
decrementTotalQuotesCount merchantId merchantOpCityId driverId sreqId = do
  mbSreqCounted <- find (\(srId, (_, isCounted)) -> srId == sreqId.getId && isCounted) <$> getSearchRequestInfoMap merchantId driverId
  whenJust mbSreqCounted $
    const . Redis.withCrossAppRedis $ withAcceptanceRatioWindowOption merchantOpCityId $ SWC.decrementWindowCount (mkTotalQuotesKey driverId.getId)

incrementTotalQuotesCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  SearchRequest ->
  UTCTime ->
  ExpirationTime ->
  m ()
incrementTotalQuotesCount merchantId merchantOpCityId driverId searchReq validTill singleBatchProcessTime = do
  now <- getCurrentTime
  srCount <- getValidSearchRequestCount merchantId (cast driverId) now
  Redis.withCrossAppRedis $ withMinQuotesToQualifyIntelligentPoolWindowOption merchantOpCityId $ SWC.incrementWindowCount (mkQuotesCountKey driverId.getId) -- total quotes sent count in different sliding window (used in driver pool for random vs intelligent filtering)
  let shouldCount = srCount < 1
  addSearchRequestInfoToCache searchReq.id merchantId (cast driverId) (validTill, shouldCount) singleBatchProcessTime
  when shouldCount $
    Redis.withCrossAppRedis $ withAcceptanceRatioWindowOption merchantOpCityId $ SWC.incrementWindowCount (mkTotalQuotesKey driverId.getId) -- for acceptance ratio calculation

mkParallelSearchRequestKey :: Id DM.Merchant -> Id DP.Driver -> Text
mkParallelSearchRequestKey mId dId = "driver-offer:DriverPool:Search-Req-Validity-Map-" <> mId.getId <> dId.getId

addSearchRequestInfoToCache ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id SearchRequest ->
  Id DM.Merchant ->
  Id DP.Driver ->
  (UTCTime, Bool) ->
  Redis.ExpirationTime ->
  m ()
addSearchRequestInfoToCache searchReqId merchantId driverId valueToPut singleBatchProcessTime = do
  Redis.withCrossAppRedis $ Redis.hSetExp (mkParallelSearchRequestKey merchantId driverId) searchReqId.getId valueToPut singleBatchProcessTime

getSearchRequestInfoMap ::
  Redis.HedisFlow m r =>
  Id DM.Merchant ->
  Id DP.Driver ->
  m [(Text, (UTCTime, Bool))]
getSearchRequestInfoMap mId dId = Redis.withCrossAppRedis $ Redis.hGetAll $ mkParallelSearchRequestKey mId dId

getValidSearchRequestCount ::
  Redis.HedisFlow m r =>
  Id DM.Merchant ->
  Id DP.Driver ->
  UTCTime ->
  m Int
getValidSearchRequestCount merchantId driverId now = Redis.withCrossAppRedis $ do
  searchRequestValidityMap <- getSearchRequestInfoMap merchantId driverId
  let (valid, old) = partition ((> now) . fst . snd) searchRequestValidityMap
  when (notNull old) $ Redis.hDel (mkParallelSearchRequestKey merchantId driverId) (map fst old)
  pure $ length valid

removeSearchReqIdFromMap ::
  ( Redis.HedisFlow m r,
    MonadTime m
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  Id SearchTry ->
  m ()
removeSearchReqIdFromMap merchantId driverId = Redis.withCrossAppRedis . Redis.hDel (mkParallelSearchRequestKey merchantId $ cast driverId) . (: []) .(.getId)

incrementQuoteAcceptedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m ()
incrementQuoteAcceptedCount merchantOpCityId driverId = Redis.withCrossAppRedis . withAcceptanceRatioWindowOption merchantOpCityId $ SWC.incrementWindowCount (mkQuotesAcceptedKey driverId.getId)

getTotalQuotesSent ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m Int
getTotalQuotesSent merchantOpCityId driverId =
  sum . catMaybes <$> (Redis.withCrossAppRedis . withAcceptanceRatioWindowOption merchantOpCityId $ SWC.getCurrentWindowValues (mkTotalQuotesKey driverId.getId))

getLatestAcceptanceRatio ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  m Double
getLatestAcceptanceRatio merchantOpCityId driverId = Redis.withCrossAppRedis . withAcceptanceRatioWindowOption merchantOpCityId $ SWC.getLatestRatio (getId driverId) mkQuotesAcceptedKey mkTotalQuotesKey (mkOldRatioKey "AcceptanceRatio")

incrementTotalRidesCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m ()
incrementTotalRidesCount merchantOpCityId driverId = Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantOpCityId $ SWC.incrementWindowCount (mkTotalRidesKey driverId.getId)

getTotalRidesCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  m Int
getTotalRidesCount merchantOpCityId driverId = sum . catMaybes <$> (Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantOpCityId $ SWC.getCurrentWindowValues (mkTotalRidesKey driverId.getId))

incrementCancellationCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m ()
incrementCancellationCount merchantOpCityId driverId = Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantOpCityId $ SWC.incrementWindowCount (mkRideCancelledKey driverId.getId)

getLatestCancellationRatio' ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  m Double
getLatestCancellationRatio' merchantOpCityId driverId = Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantOpCityId $ SWC.getLatestRatio driverId.getId mkRideCancelledKey mkTotalRidesKey (mkOldRatioKey "CancellationRatio")

getLatestCancellationRatio ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  CancellationScoreRelatedConfig ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  m Double
getLatestCancellationRatio cancellationScoreRelatedConfig merchantOpCityId driverId = do
  isThresholdRidesDone <- isThresholdRidesCompleted driverId merchantOpCityId cancellationScoreRelatedConfig
  if isThresholdRidesDone
    then getLatestCancellationRatio' merchantOpCityId driverId
    else pure 0

getCurrentWindowAvailability ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    FromJSON a
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  m [Maybe a]
getCurrentWindowAvailability merchantOpCityId driverId = Redis.withCrossAppRedis . withAvailabilityTimeWindowOption merchantOpCityId $ SWC.getCurrentWindowValues (mkAvailableTimeKey driverId.getId)

mkQuotesCountKey :: Text -> Text
mkQuotesCountKey driverId = "driver-offer:DriverPool:Total-quotes-sent:DriverId-" <> driverId

getQuotesCount ::
  ( FromJSON a,
    CacheFlow m r,
    EsqDBFlow m r,
    Num a
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  m a
getQuotesCount merchantOpCityId driverId = sum . catMaybes <$> (Redis.withCrossAppRedis . withMinQuotesToQualifyIntelligentPoolWindowOption merchantOpCityId $ SWC.getCurrentWindowValues (mkQuotesCountKey driverId.getId))

isThresholdRidesCompleted ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DP.Driver ->
  Id DMOC.MerchantOperatingCity ->
  CancellationScoreRelatedConfig ->
  m Bool
isThresholdRidesCompleted driverId merchantOpCityId cancellationScoreRelatedConfig = do
  let minRidesForCancellationScore = fromMaybe 5 cancellationScoreRelatedConfig.minRidesForCancellationScore
  totalRides <- getTotalRidesCount merchantOpCityId driverId
  pure $ totalRides >= minRidesForCancellationScore

getPopupDelay ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  Double ->
  CancellationScoreRelatedConfig ->
  Seconds ->
  m Seconds
getPopupDelay merchantOpCityId driverId cancellationRatio cancellationScoreRelatedConfig defaultPopupDelay = do
  let cancellationRatioThreshold = fromIntegral $ fromMaybe 40 cancellationScoreRelatedConfig.thresholdCancellationScore
  (defaultPopupDelay +)
    <$> if cancellationRatio * 100 > cancellationRatioThreshold
      then do
        isThresholdRidesDone <- isThresholdRidesCompleted driverId merchantOpCityId cancellationScoreRelatedConfig
        pure $
          if isThresholdRidesDone
            then fromMaybe (Seconds 0) cancellationScoreRelatedConfig.popupDelayToAddAsPenalty
            else Seconds 0
      else pure $ Seconds 0

mkDriverLocationUpdatesKey :: Id DMOC.MerchantOperatingCity -> Id DP.Person -> Text
mkDriverLocationUpdatesKey mocId dId = "driver-offer:DriverPool:mocId-" <> mocId.getId <> ":dId:" <> dId.getId

updateDriverSpeedInRedis ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  LatLong ->
  UTCTime ->
  m ()
updateDriverSpeedInRedis merchantOpCityId driverId points timeStamp = Redis.withCrossAppRedis $ do
  locationUpdateSampleTime <- maybe 3 (.locationUpdateSampleTime) <$> DIP.findByMerchantOpCityId merchantOpCityId
  now <- getCurrentTime
  let driverLocationUpdatesKey = mkDriverLocationUpdatesKey merchantOpCityId driverId
  locationUpdatesList :: [(LatLong, UTCTime)] <-
    sortOn (Down . snd)
      . ((points, timeStamp) :)
      . filter
        ( \(_, time) ->
            time > addUTCTime (fromIntegral $ (-60) * locationUpdateSampleTime.getMinutes) now
        )
      . concat
      <$> Redis.safeGet driverLocationUpdatesKey
  Redis.set driverLocationUpdatesKey locationUpdatesList

getDriverAverageSpeed ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m Double
getDriverAverageSpeed merchantOpCityId driverId = Redis.withCrossAppRedis $ do
  intelligentPoolConfig <- DIP.findByMerchantOpCityId merchantOpCityId
  let minLocationUpdates = maybe 3 (.minLocationUpdates) intelligentPoolConfig
      defaultDriverSpeed = maybe 27.0 (.defaultDriverSpeed) intelligentPoolConfig
  let driverLocationUpdatesKey = mkDriverLocationUpdatesKey merchantOpCityId driverId
  locationUpdatesList :: [(LatLong, UTCTime)] <- concat <$> Redis.safeGet driverLocationUpdatesKey
  let locationUpdatesCount = length locationUpdatesList
  if locationUpdatesCount > minLocationUpdates
    then do
      let locationUpdatesPairs = zip (drop 1 locationUpdatesList) (take (locationUpdatesCount - 1) locationUpdatesList)
          (totalDistanceTravelled, totalTimeTaken) :: (HighPrecMeters, Centi) =
            foldr
              ( \((locationA, timeA), (locationB, timeB)) (accDis, accTime) -> do
                  let distance = CD.distanceBetweenInMeters locationB locationA
                      timeTaken = fromInteger . floor $ diffUTCTime timeB timeA
                  (accDis + distance, accTime + timeTaken)
              )
              (0, 0)
              locationUpdatesPairs
      pure . fromRational . toRational $ totalDistanceTravelled.getHighPrecMeters.getCenti / totalTimeTaken
    else pure defaultDriverSpeed

mkBlockListedDriversKey :: Id SearchRequest -> Text
mkBlockListedDriversKey searchReqId = "Block-Listed-Drivers-Key:SearchRequestId-" <> searchReqId.getId

calculateGoHomeDriverPool ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    MonadIO m,
    HasCoordinates a,
    LT.HasLocationService m r,
    CoreMetrics m
  ) =>
  CalculateGoHomeDriverPoolReq a ->
  Id DMOC.MerchantOperatingCity ->
  m [DriverPoolWithActualDistResult]
calculateGoHomeDriverPool CalculateGoHomeDriverPoolReq {..} merchantOpCityId = do
  now <- getCurrentTime
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QP.getNearestGoHomeDrivers $
        QP.NearestGoHomeDriversReq
          { variant = variant,
            fromLocation = getCoordinates fromLocation,
            nearestRadius = goHomeCfg.goHomeFromLocationRadius,
            homeRadius = goHomeCfg.goHomeWayPointRadius,
            merchantId,
            driverPositionInfoExpiry = driverPoolCfg.driverPositionInfoExpiry
          }
  driversWithLessThanNParallelRequests <- case poolStage of
    DriverSelection -> filterM (fmap (< driverPoolCfg.maxParallelSearchRequests) . getParallelSearchRequestCount now) approxDriverPool
    Estimate -> pure approxDriverPool --estimate stage we dont need to consider actual parallel request counts
  randomDriverPool <- liftIO $ take goHomeCfg.numDriversForDirCheck <$> Rnd.randomizeList driversWithLessThanNParallelRequests
  logDebug $ "random driver pool" <> show randomDriverPool
  goHomeRequests <-
    mapMaybeM
      ( \driver -> runMaybeT $ do
          ghrId <- MaybeT $ CQDGR.getDriverGoHomeRequestInfo driver.driverId merchantOpCityId Nothing <&> (.driverGoHomeRequestId)
          goHomeReq <- MaybeT $ QDGR.findById ghrId
          return (goHomeReq, driver)
      )
      randomDriverPool
  merchant <- CTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigDoesNotExist merchantId.getId)

  let convertedDriverPoolRes = map (\(ghr, driver) -> (ghr,driver,) $ makeDriverPoolRes driver) goHomeRequests
  driverGoHomePoolWithActualDistance <-
    case convertedDriverPoolRes of
      [] -> return []
      _ -> do
        driverGoHomePoolWithActualDistance <- zipWith (curry (\((ghr, driver, _), dpwAD) -> (ghr, driver, dpwAD))) convertedDriverPoolRes . NE.toList <$> computeActualDistance merchantId merchantOpCityId fromLocation (NE.fromList $ map (\(_, _, c) -> c) convertedDriverPoolRes)
        case driverPoolCfg.actualDistanceThreshold of
          Nothing -> return driverGoHomePoolWithActualDistance
          Just threshold -> do
            logDebug $ "Threshold :" <> show threshold
            let res = filter (\(_, _, dpwAD) -> filterFunc threshold dpwAD) driverGoHomePoolWithActualDistance
            logDebug $ "secondly filtered go home driver pool" <> show (map snd3 res)
            return res

  driversRoutes' <- getRoutesForAllDrivers driverGoHomePoolWithActualDistance
  let driversRoutes = map (refactorRoutesResp goHomeCfg) driversRoutes'
  let driversOnWayToHome =
        filter
          ( \(_, driverRoute, _, _) ->
              any (\wp -> highPrecMetersToMeters (distanceBetweenInMeters (getCoordinates toLocation) wp) <= goHomeCfg.goHomeWayPointRadius) driverRoute.points
          )
          driversRoutes

  let goHomeDriverPoolWithActualDist = makeDriverPoolWithActualDistResult merchant <$> driversOnWayToHome
  return $ take driverPoolCfg.driverBatchSize goHomeDriverPoolWithActualDist
  where
    filterFunc threshold estDist = getMeters estDist.actualDistanceToPickup <= fromIntegral threshold
    getParallelSearchRequestCount now dObj = getValidSearchRequestCount merchantId (dObj.driverId) now

    makeDriverPoolRes nearestGoHomeDrivers =
      DriverPoolResult
        { driverId = nearestGoHomeDrivers.driverId,
          language = nearestGoHomeDrivers.language,
          driverDeviceToken = nearestGoHomeDrivers.driverDeviceToken,
          distanceToPickup = nearestGoHomeDrivers.distanceToDriver,
          variant = nearestGoHomeDrivers.variant,
          lat = nearestGoHomeDrivers.lat,
          lon = nearestGoHomeDrivers.lon,
          mode = nearestGoHomeDrivers.mode
        }

    getRoutesForAllDrivers =
      mapM
        ( \(ghReq, driver, driverGoHomePoolWithActualDistance) -> do
            routes <-
              DRoute.getTripRoutes (driver.driverId, merchantId, merchantOpCityId) $
                Maps.GetRoutesReq
                  { waypoints = getCoordinates driver :| [getCoordinates ghReq],
                    mode = Just Maps.CAR,
                    calcPoints = True
                  }
            let route = if null routes then defRouteInfo else head routes
            return (driver, route, ghReq.id, driverGoHomePoolWithActualDistance)
        )

    defRouteInfo =
      RouteInfo
        { duration = Nothing,
          distance = Nothing,
          boundingBox = Nothing,
          snappedWaypoints = [],
          points = []
        }

    makeDriverPoolWithActualDistResult merchant (driverPoolRes, _, ghrId, driverGoHomePoolWithActualDistance) = do
      DriverPoolWithActualDistResult
        { driverPoolResult = makeDriverPoolResult driverPoolRes,
          actualDistanceToPickup = driverGoHomePoolWithActualDistance.actualDistanceToPickup, --fromMaybe 0 driverRoute.distance,
          actualDurationToPickup = driverGoHomePoolWithActualDistance.actualDurationToPickup,
          intelligentScores = IntelligentScores Nothing Nothing Nothing Nothing Nothing merchant.defaultPopupDelay,
          isPartOfIntelligentPool = False,
          keepHiddenForSeconds = Seconds 0,
          goHomeReqId = Just ghrId
        }

    makeDriverPoolResult QP.NearestGoHomeDriversResult {variant = variant', ..} =
      DriverPoolResult
        { distanceToPickup = distanceToDriver,
          variant = variant',
          ..
        }

calculateDriverPool ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasCoordinates a,
    LT.HasLocationService m r
  ) =>
  PoolCalculationStage ->
  DriverPoolConfig ->
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Bool ->
  Maybe PoolRadiusStep ->
  m [DriverPoolResult]
calculateDriverPool poolStage driverPoolCfg mbVariant pickup merchantId onlyNotOnRide mRadiusStep = do
  let radius = getRadius mRadiusStep
  let coord = getCoordinates pickup
  now <- getCurrentTime
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QPG.getNearestDrivers
        mbVariant
        coord
        radius
        merchantId
        onlyNotOnRide
        driverPoolCfg.driverPositionInfoExpiry
  driversWithLessThanNParallelRequests <- case poolStage of
    DriverSelection -> filterM (fmap (< driverPoolCfg.maxParallelSearchRequests) . getParallelSearchRequestCount now) approxDriverPool
    Estimate -> pure approxDriverPool --estimate stage we dont need to consider actual parallel request counts
  let driverPoolResult = makeDriverPoolResult <$> driversWithLessThanNParallelRequests
  logDebug $ "driverPoolResult: " <> show driverPoolResult
  pure driverPoolResult
  where
    getParallelSearchRequestCount now dObj = getValidSearchRequestCount merchantId (cast dObj.driverId) now
    getRadius mRadiusStep_ = do
      let maxRadius = driverPoolCfg.maxRadiusOfSearch
      case mRadiusStep_ of
        Just radiusStep -> do
          let minRadius = driverPoolCfg.minRadiusOfSearch
          let radiusStepSize = driverPoolCfg.radiusStepSize
          min (minRadius + radiusStepSize * radiusStep) maxRadius
        Nothing -> maxRadius
    makeDriverPoolResult :: QP.NearestDriversResult -> DriverPoolResult
    makeDriverPoolResult QP.NearestDriversResult {..} = do
      DriverPoolResult
        { distanceToPickup = distanceToDriver,
          variant = variant,
          ..
        }

calculateDriverPoolWithActualDist ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    LT.HasLocationService m r
  ) =>
  PoolCalculationStage ->
  DriverPoolConfig ->
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Bool ->
  Maybe PoolRadiusStep ->
  m [DriverPoolWithActualDistResult]
calculateDriverPoolWithActualDist poolCalculationStage driverPoolCfg mbVariant pickup merchantId merchantOpCityId onlyNotOnRide mRadiusStep = do
  driverPool <- calculateDriverPool poolCalculationStage driverPoolCfg mbVariant pickup merchantId onlyNotOnRide mRadiusStep
  case driverPool of
    [] -> return []
    (a : pprox) -> do
      driverPoolWithActualDist <- computeActualDistance merchantId merchantOpCityId pickup (a :| pprox)
      let filtDriverPoolWithActualDist = case driverPoolCfg.actualDistanceThreshold of
            Nothing -> NE.toList driverPoolWithActualDist
            Just threshold -> NE.filter (filterFunc threshold) driverPoolWithActualDist
      logDebug $ "secondly filtered driver pool" <> show filtDriverPoolWithActualDist
      return filtDriverPoolWithActualDist
  where
    filterFunc threshold estDist = getMeters estDist.actualDistanceToPickup <= fromIntegral threshold

calculateDriverPoolCurrentlyOnRide ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    HasCoordinates a,
    LT.HasLocationService m r,
    CoreMetrics m
  ) =>
  PoolCalculationStage ->
  DriverPoolConfig ->
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Maybe PoolRadiusStep ->
  m [DriverPoolResultCurrentlyOnRide]
calculateDriverPoolCurrentlyOnRide poolStage driverPoolCfg mbVariant pickup merchantId mRadiusStep = do
  let radius = getRadius mRadiusStep
  let coord = getCoordinates pickup
  now <- getCurrentTime
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPoolCurrentlyOnRide" $
      B.runInReplica $
        QP.getNearestDriversCurrentlyOnRide
          mbVariant
          coord
          radius
          merchantId
          driverPoolCfg.driverPositionInfoExpiry
          driverPoolCfg.radiusShrinkValueForDriversOnRide
  driversWithLessThanNParallelRequests <- case poolStage of
    DriverSelection -> filterM (fmap (< driverPoolCfg.maxParallelSearchRequests) . getParallelSearchRequestCount now) approxDriverPool
    Estimate -> pure approxDriverPool --estimate stage we dont need to consider actual parallel request counts
  pure $ makeDriverPoolResult <$> driversWithLessThanNParallelRequests
  where
    getParallelSearchRequestCount now dObj = getValidSearchRequestCount merchantId (cast dObj.driverId) now
    getRadius mRadiusStep_ = do
      let maxRadius = driverPoolCfg.maxRadiusOfSearch
      case mRadiusStep_ of
        Just radiusStep -> do
          let minRadius = driverPoolCfg.minRadiusOfSearch
          let radiusStepSize = driverPoolCfg.radiusStepSize
          min (minRadius + radiusStepSize * radiusStep) maxRadius
        Nothing -> maxRadius
    makeDriverPoolResult :: QP.NearestDriversResultCurrentlyOnRide -> DriverPoolResultCurrentlyOnRide
    makeDriverPoolResult QP.NearestDriversResultCurrentlyOnRide {..} =
      DriverPoolResultCurrentlyOnRide
        { distanceToPickup = distanceToDriver,
          ..
        }

calculateDriverCurrentlyOnRideWithActualDist ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasCoordinates a,
    LT.HasLocationService m r,
    CoreMetrics m
  ) =>
  PoolCalculationStage ->
  DriverPoolConfig ->
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe PoolRadiusStep ->
  m [DriverPoolWithActualDistResult]
calculateDriverCurrentlyOnRideWithActualDist poolCalculationStage driverPoolCfg mbVariant pickup merchantId merchantOpCityId mRadiusStep = do
  driverPool <- calculateDriverPoolCurrentlyOnRide poolCalculationStage driverPoolCfg mbVariant pickup merchantId mRadiusStep
  case driverPool of
    [] -> return []
    (a : pprox) -> do
      let driverPoolResultsWithDriverLocationAsDestinationLocation = driverResultFromDestinationLocation <$> (a :| pprox)
          driverToDestinationDistanceThreshold = driverPoolCfg.driverToDestinationDistanceThreshold
      driverPoolWithActualDistFromDestinationLocation <- computeActualDistance merchantId merchantOpCityId pickup driverPoolResultsWithDriverLocationAsDestinationLocation
      driverPoolWithActualDistFromCurrentLocation <- traverse (calculateActualDistanceCurrently driverToDestinationDistanceThreshold) (a :| pprox)
      let driverPoolWithActualDist = NE.zipWith (curry combine) driverPoolWithActualDistFromDestinationLocation driverPoolWithActualDistFromCurrentLocation
          filtDriverPoolWithActualDist = case driverPoolCfg.actualDistanceThreshold of
            Nothing -> NE.toList driverPoolWithActualDist
            Just threshold -> NE.filter (filterFunc threshold) driverPoolWithActualDist
      logDebug $ "secondly filtered driver pool result currently on ride" <> show filtDriverPoolWithActualDist
      return filtDriverPoolWithActualDist
  where
    filterFunc threshold estDist = getMeters estDist.actualDistanceToPickup <= fromIntegral threshold
    driverResultFromDestinationLocation DriverPoolResultCurrentlyOnRide {..} =
      DriverPoolResult
        { driverId = driverId,
          language = language,
          driverDeviceToken = driverDeviceToken,
          distanceToPickup = distanceToPickup,
          variant = variant,
          lat = destinationLat,
          lon = destinationLon,
          mode = mode
        }
    calculateActualDistanceCurrently driverToDestinationDistanceThreshold DriverPoolResultCurrentlyOnRide {..} = do
      let temp = DriverPoolResult {..}
      if distanceFromDriverToDestination < driverToDestinationDistanceThreshold
        then do
          transporter <- CTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
          let defaultPopupDelay = fromMaybe 2 transporter.popupDelayToAddAsPenalty
          let time = driverPoolCfg.driverToDestinationDuration
          pure
            DriverPoolWithActualDistResult
              { driverPoolResult = temp,
                actualDistanceToPickup = distanceFromDriverToDestination,
                actualDurationToPickup = time,
                intelligentScores = IntelligentScores Nothing Nothing Nothing Nothing Nothing defaultPopupDelay,
                isPartOfIntelligentPool = False,
                keepHiddenForSeconds = Seconds 0,
                goHomeReqId = Nothing
              }
        else computeActualDistanceOneToOne merchantId merchantOpCityId (LatLong destinationLat destinationLon) temp
    combine (DriverPoolWithActualDistResult {actualDistanceToPickup = x, actualDurationToPickup = y}, DriverPoolWithActualDistResult {..}) =
      DriverPoolWithActualDistResult
        { actualDistanceToPickup = x + actualDistanceToPickup,
          actualDurationToPickup = y + actualDurationToPickup,
          ..
        }

computeActualDistanceOneToOne ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasCoordinates a
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  a ->
  DriverPoolResult ->
  m DriverPoolWithActualDistResult
computeActualDistanceOneToOne merchantId merchantOpCityId pickup driverPoolResult = do
  (ele :| _) <- computeActualDistance merchantId merchantOpCityId pickup (driverPoolResult :| [])
  pure ele

computeActualDistance ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasCoordinates a
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  a ->
  NonEmpty DriverPoolResult ->
  m (NonEmpty DriverPoolWithActualDistResult)
computeActualDistance orgId merchantOpCityId pickup driverPoolResults = do
  let pickupLatLong = getCoordinates pickup
  transporter <- CTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId.getId)
  getDistanceResults <-
    Maps.getEstimatedPickupDistances orgId merchantOpCityId $
      Maps.GetDistancesReq
        { origins = driverPoolResults,
          destinations = pickupLatLong :| [],
          travelMode = Just Maps.CAR
        }
  logDebug $ "get distance results" <> show getDistanceResults
  return $ mkDriverPoolWithActualDistResult transporter.defaultPopupDelay <$> getDistanceResults
  where
    mkDriverPoolWithActualDistResult defaultPopupDelay distDur = do
      DriverPoolWithActualDistResult
        { driverPoolResult = distDur.origin,
          actualDistanceToPickup = distDur.distance,
          actualDurationToPickup = distDur.duration,
          intelligentScores = IntelligentScores Nothing Nothing Nothing Nothing Nothing defaultPopupDelay,
          isPartOfIntelligentPool = False,
          keepHiddenForSeconds = Seconds 0,
          goHomeReqId = Nothing
        }

refactorRoutesResp :: GoHomeConfig -> (QP.NearestGoHomeDriversResult, Maps.RouteInfo, Id DDGR.DriverGoHomeRequest, DriverPoolWithActualDistResult) -> (QP.NearestGoHomeDriversResult, Maps.RouteInfo, Id DDGR.DriverGoHomeRequest, DriverPoolWithActualDistResult)
refactorRoutesResp goHomeCfg (nearestDriverRes, route, ghrId, driverGoHomePoolWithActualDistance) = (nearestDriverRes, newRoute route, ghrId, driverGoHomePoolWithActualDistance)
  where
    newRoute route' =
      RouteInfo
        { distance = route'.distance,
          duration = route'.duration,
          points = getStartPoint $ filterInitPoints (refactor [] route'.points),
          snappedWaypoints = route'.snappedWaypoints,
          boundingBox = route'.boundingBox
        }

    filterInitPoints (x1 : x2 : xs) = if highPrecMetersToMeters (distanceBetweenInMeters x1 x2) <= goHomeCfg.ignoreWaypointsTill then filterInitPoints (x1 : xs) else x1 : x2 : xs
    filterInitPoints [x] = [x]
    filterInitPoints [] = []

    getStartPoint (x1 : x2 : xs) = getPointInBetween x1 x2 (fromIntegral (getMeters goHomeCfg.addStartWaypointAt) / 111000) : x2 : xs -- 1 degree = 111 Km
    getStartPoint [x] = [x]
    getStartPoint [] = []

    refactor acc (p1 : p2 : ps) = if highPrecMetersToMeters (distanceBetweenInMeters p1 p2) > goHomeCfg.goHomeWayPointRadius then refactor (p1 : acc) (getPointInBetween p1 p2 (fromIntegral (goHomeCfg.goHomeWayPointRadius.getMeters) / 111000) : p2 : ps) else refactor (p1 : acc) (p2 : ps)
    refactor acc [p1] = reverse (p1 : acc)
    refactor _ [] = []

    getPointInBetween p1 p2 dist = LatLong {lat = p1.lat + dist * (p2.lat - p1.lat) / d, lon = p1.lon + dist * (p2.lon - p1.lon) / d}
      where
        d = sqrt ((p2.lat - p1.lat) ^ (2 :: Int) + (p2.lon - p1.lon) ^ (2 :: Int))
