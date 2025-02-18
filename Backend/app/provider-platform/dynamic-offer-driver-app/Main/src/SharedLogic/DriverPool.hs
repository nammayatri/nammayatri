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
    getTotalRidesCount,
    getValidSearchRequestCount,
    removeSearchReqIdFromMap,
    updateDriverSpeedInRedis,
    getDriverAverageSpeed,
    mkAvailableTimeKey,
    mkBlockListedDriversKey,
    mkBlockListedDriversForRiderKey,
    addDriverToSearchCancelledList,
    addDriverToRiderCancelledList,
    convertDriverPoolWithActualDistResultToNearestGoHomeDriversResult,
    filterOutGoHomeDriversAccordingToHomeLocation,
    PoolCalculationStage (..),
    CalculateDriverPoolReq (..),
    module Reexport,
    scheduledRideFilter,
    getVehicleAvgSpeed,
    getBatchSize,
  )
where

import Control.Monad.Extra (mapMaybeM)
import Data.Fixed
import qualified Data.Geohash as DG
import Data.List (find, partition)
import Data.List.Extra (notNull)
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Extra as NE
import qualified Data.Text as T
import Data.Tuple.Extra (snd3)
import qualified Data.Vector as V
import Domain.Action.UI.Route as DRoute
import Domain.Types as DVST
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.DriverIntelligentPoolConfig (IntelligentScores (IntelligentScores))
import qualified Domain.Types.DriverIntelligentPoolConfig as DIPC
import Domain.Types.DriverPoolConfig
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.RiderDetails (RiderDetails)
import Domain.Types.SearchRequest
import Domain.Types.SearchTry
import qualified Domain.Types.TransporterConfig as DTC
import Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as DVeh
import EulerHS.Prelude hiding (find, id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Types
import Kernel.Prelude (NominalDiffTime, head, listToMaybe)
import qualified Kernel.Prelude as KP
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
import qualified SharedLogic.Beckn.Common as DST
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Cac.DriverIntelligentPoolConfig as CDIP
import Storage.Cac.DriverPoolConfig as Reexport
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person.GetNearestDrivers as QPG
import qualified Storage.Queries.Transformers.DriverInformation as TDI
import Tools.Maps as Maps
import qualified Tools.Maps as TMaps
import Tools.Metrics
import Utils.Common.Cac.KeyNameConstants

mkTotalQuotesKey :: Text -> Text
mkTotalQuotesKey driverId = "driver-offer:DriverPool:Total-quotes:DriverId-" <> driverId

mkQuotesAcceptedKey :: Text -> Text
mkQuotesAcceptedKey driverId = "driver-offer:DriverPool:Quote-accepted:DriverId-" <> driverId

mkTotalRidesKey :: Text -> Text
mkTotalRidesKey driverId = "driver-offer:DriverPool:Total-Rides:DriverId-" <> driverId

mkRideCancelledKey :: Text -> Text
mkRideCancelledKey driverId = "driver-offer:DriverPool:Ride-cancelled:DriverId-" <> driverId

mkAvailableTimeKey :: Text -> Text
mkAvailableTimeKey driverId = "driver-offer:DriverPool:Available-Time:DriverId-" <> driverId

windowFromIntelligentPoolConfig :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> (DIPC.DriverIntelligentPoolConfig -> SWC.SlidingWindowOptions) -> m SWC.SlidingWindowOptions
windowFromIntelligentPoolConfig merchantOpCityId windowKey = maybe defaultWindow windowKey <$> CDIP.findByMerchantOpCityId merchantOpCityId Nothing
  where
    defaultWindow = SWC.SlidingWindowOptions 7 SWC.Days

getBatchSize :: V.Vector Int -> Int -> Int -> Int
getBatchSize dynamicBatchSize index driverBatchSize =
  let size = min (V.length dynamicBatchSize - 1) (index + 1)
   in bool (dynamicBatchSize V.! size) driverBatchSize (size <= -1)

withAcceptanceRatioWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withAcceptanceRatioWindowOption merchantOpCityId fn = windowFromIntelligentPoolConfig merchantOpCityId (.acceptanceRatioWindowOption) >>= fn

withCancellationAndRideFrequencyRatioWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withCancellationAndRideFrequencyRatioWindowOption merchantOpCityId fn = windowFromIntelligentPoolConfig merchantOpCityId (.cancellationAndRideFrequencyRatioWindowOption) >>= fn

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
getTotalQuotesSent merchantOpCityId driverId = fmap fromIntegral . Redis.withCrossAppRedis . withAcceptanceRatioWindowOption merchantOpCityId $ SWC.getCurrentWindowCount (mkTotalQuotesKey driverId.getId)

getLatestAcceptanceRatio ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  m Double
getLatestAcceptanceRatio merchantOpCityId driverId = Redis.withCrossAppRedis . withAcceptanceRatioWindowOption merchantOpCityId $ SWC.getLatestRatio (getId driverId) mkQuotesAcceptedKey mkTotalQuotesKey

incrementTotalRidesCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m ()
incrementTotalRidesCount merchantOpCityId driverId = Redis.withCrossAppRedis . withCancellationAndRideFrequencyRatioWindowOption merchantOpCityId $ SWC.incrementWindowCount (mkTotalRidesKey driverId.getId)

getTotalRidesCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  m Int
getTotalRidesCount merchantOpCityId driverId = fmap fromIntegral . Redis.withCrossAppRedis . withCancellationAndRideFrequencyRatioWindowOption merchantOpCityId $ SWC.getCurrentWindowCount (mkTotalRidesKey driverId.getId)

incrementCancellationCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m ()
incrementCancellationCount merchantOpCityId driverId = Redis.withCrossAppRedis . withCancellationAndRideFrequencyRatioWindowOption merchantOpCityId $ SWC.incrementWindowCount (mkRideCancelledKey driverId.getId)

getLatestCancellationRatio' ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  m Double
getLatestCancellationRatio' merchantOpCityId driverId = Redis.withCrossAppRedis . withCancellationAndRideFrequencyRatioWindowOption merchantOpCityId $ SWC.getLatestRatio driverId.getId mkRideCancelledKey mkTotalRidesKey

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
getQuotesCount merchantOpCityId driverId = fmap fromIntegral . Redis.withCrossAppRedis . withMinQuotesToQualifyIntelligentPoolWindowOption merchantOpCityId $ SWC.getCurrentWindowCount (mkQuotesCountKey driverId.getId)

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
  locationUpdateSampleTime <- maybe 3 (.locationUpdateSampleTime) <$> CDIP.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
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
  intelligentPoolConfig <- CDIP.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
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

mkBlockListedDriversForRiderKey :: Id RiderDetails -> Text
mkBlockListedDriversForRiderKey riderId = "Block-Listed-Drivers-Key:RiderId-" <> riderId.getId

addDriverToSearchCancelledList ::
  ( CacheFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  Id SearchRequest ->
  Id DP.Person ->
  m ()
addDriverToSearchCancelledList searchReqId driverId = do
  let keyForDriverCancelledList = mkBlockListedDriversKey searchReqId
  cacheBlockListedDrivers keyForDriverCancelledList driverId

addDriverToRiderCancelledList ::
  ( CacheFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  Id DP.Person ->
  Id RiderDetails ->
  m ()
addDriverToRiderCancelledList driverId riderId = do
  let keyForDriverCancelledList = mkBlockListedDriversForRiderKey riderId
  cacheBlockListedDrivers keyForDriverCancelledList driverId

cacheBlockListedDrivers ::
  ( CacheFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  Text ->
  Id DP.Person ->
  m ()
cacheBlockListedDrivers key driverId = do
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  Redis.withCrossAppRedis $ Redis.rPushExp key [driverId] (round searchRequestExpirationSeconds)

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
calculateGoHomeDriverPool req@CalculateGoHomeDriverPoolReq {..} merchantOpCityId = do
  now <- getCurrentTime
  cityServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QP.getNearestGoHomeDrivers $
        QP.NearestGoHomeDriversReq
          { cityServiceTiers,
            serviceTiers,
            fromLocation = getCoordinates fromLocation,
            nearestRadius = goHomeCfg.goHomeFromLocationRadius,
            homeRadius = goHomeCfg.goHomeWayPointRadius,
            merchantId,
            driverPositionInfoExpiry = driverPoolCfg.driverPositionInfoExpiry,
            isRental,
            isInterCity,
            onlinePayment,
            now,
            isValueAddNP
          }
  driversWithLessThanNParallelRequests <- case poolStage of
    DriverSelection -> filterM (fmap (< driverPoolCfg.maxParallelSearchRequests) . getParallelSearchRequestCount now) approxDriverPool
    Estimate -> pure approxDriverPool --estimate stage we dont need to consider actual parallel request counts
  randomDriverPool <- liftIO $ take goHomeCfg.numDriversForDirCheck <$> Rnd.randomizeList driversWithLessThanNParallelRequests
  logDebug $ "random driver pool" <> show randomDriverPool
  fst <$> filterOutGoHomeDriversAccordingToHomeLocation randomDriverPool req merchantOpCityId
  where
    getParallelSearchRequestCount now dObj = getValidSearchRequestCount merchantId (dObj.driverId) now

convertDriverPoolWithActualDistResultToNearestGoHomeDriversResult :: Bool -> Bool -> DriverPoolWithActualDistResult -> QP.NearestGoHomeDriversResult -- # TODO: Lets merge these two types
convertDriverPoolWithActualDistResultToNearestGoHomeDriversResult onRide isSpecialLocWarrior DriverPoolWithActualDistResult {driverPoolResult = DriverPoolResult {..}} = do
  QP.NearestGoHomeDriversResult {QP.distanceToDriver = distanceToPickup, ..}

-- this is not required in the flow where we convert them

filterOutGoHomeDriversAccordingToHomeLocation ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    MonadIO m,
    HasCoordinates a,
    LT.HasLocationService m r,
    CoreMetrics m
  ) =>
  [QP.NearestGoHomeDriversResult] ->
  CalculateGoHomeDriverPoolReq a ->
  Id DMOC.MerchantOperatingCity ->
  m ([DriverPoolWithActualDistResult], [Id DP.Driver])
filterOutGoHomeDriversAccordingToHomeLocation randomDriverPool CalculateGoHomeDriverPoolReq {..} merchantOpCityId = do
  logDebug $ "MetroWarriorDebugging randomDriverPool -----" <> show randomDriverPool
  now <- getCurrentTime
  goHomeRequests <-
    mapMaybeM
      ( \driver -> runMaybeT $ do
          ghrId <- MaybeT $ CQDGR.getDriverGoHomeRequestInfo driver.driverId merchantOpCityId Nothing <&> (.driverGoHomeRequestId)
          goHomeReq <- MaybeT $ QDGR.findById ghrId
          return (goHomeReq, driver)
      )
      randomDriverPool
  let specialLocWarriorDrivers = filter (\driver -> driver.isSpecialLocWarrior) randomDriverPool -- specialLocWarriorDriversInfo <- Int.getSpecialLocWarriorDriverInfo specialLocWarriorDrivers
  logDebug $ "MetroWarriorDebugging specialLocWarriorDrivers -----" <> show specialLocWarriorDrivers
  specialLocgoHomeRequests <-
    mapMaybeM
      ( \specialLocWarriorDriver -> runMaybeT $ do
          driverInfo <- MaybeT $ Int.getSpecialLocWarriorDriverInfo specialLocWarriorDriver.driverId.getId
          specialLocWarriorDriverInfo <- MaybeT $ return $ if driverInfo.isSpecialLocWarrior then Just driverInfo else Nothing
          preferredLocId <- MaybeT $ return specialLocWarriorDriverInfo.preferredPrimarySpecialLocId
          preferredLoc <- MaybeT $ TDI.getPreferredPrimarySpecialLoc (Just preferredLocId.getId)
          preferredLocGate <- MaybeT $ return $ listToMaybe preferredLoc.gates
          let gHR =
                DDGR.DriverGoHomeRequest
                  { createdAt = now,
                    driverId = driverInfo.driverId,
                    id = Id "specialLocWarriorGoHomeId",
                    lat = preferredLocGate.point.lat,
                    lon = preferredLocGate.point.lon,
                    mbReachedHome = Nothing,
                    numCancellation = 0,
                    status = DDGR.ACTIVE,
                    updatedAt = now,
                    merchantId = Just merchantId,
                    merchantOperatingCityId = Just merchantOpCityId
                  }
          return (gHR, specialLocWarriorDriver)
      )
      specialLocWarriorDrivers
  logDebug $ "MetroWarriorDebugging specialLocgoHomeRequests -----" <> show specialLocgoHomeRequests
  let convertedDriverPoolRes = map (\(ghr, driver) -> (ghr,driver,) $ makeDriverPoolRes driver) (goHomeRequests <> specialLocgoHomeRequests)
  driverGoHomePoolWithActualDistance <-
    case convertedDriverPoolRes of
      [] -> return []
      _ -> do
        driverGoHomePoolWithActualDistance <- zipWith (curry (\((ghr, driver, _), dpwAD) -> (ghr, driver, dpwAD))) convertedDriverPoolRes . NE.toList <$> computeActualDistance driverPoolCfg.distanceUnit merchantId merchantOpCityId Nothing fromLocation (NE.fromList $ map (\(_, _, c) -> c) convertedDriverPoolRes)
        case driverPoolCfg.actualDistanceThreshold of
          Nothing -> return driverGoHomePoolWithActualDistance
          Just threshold -> do
            logDebug $ "Threshold :" <> show threshold
            let res = filter (\(_, driver, dpwAD) -> filterFunc threshold dpwAD driver.distanceToDriver) driverGoHomePoolWithActualDistance
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
  let goHomeDriverIdsToDest = map (\(driver, _, _, _) -> driver.driverId) driversOnWayToHome
  let goHomeDriverIdsNotToDest = map (\(_, driver, _) -> driver.driverId) $ filter (\(_, driver, _) -> driver.driverId `notElem` goHomeDriverIdsToDest) driverGoHomePoolWithActualDistance
  logDebug $ "MetroWarriorDebugging goHomeDriverIdsToDest -----" <> show goHomeDriverIdsToDest
  logDebug $ "MetroWarriorDebugging goHomeDriverIdsNotToDest -----" <> show goHomeDriverIdsNotToDest
  let goHomeDriverPoolWithActualDist = makeDriverPoolWithActualDistResult <$> driversOnWayToHome
  logDebug $ "MetroWarriorDebugging goHomeDriverPoolWithActualDist -----" <> show goHomeDriverPoolWithActualDist
  return (take (getBatchSize driverPoolCfg.dynamicBatchSize (-1) driverPoolCfg.driverBatchSize) goHomeDriverPoolWithActualDist, goHomeDriverIdsNotToDest)
  where
    filterFunc threshold estDist distanceToPickup =
      case driverPoolCfg.thresholdToIgnoreActualDistanceThreshold of
        Just thresholdToIgnoreActualDistanceThreshold -> (distanceToPickup <= thresholdToIgnoreActualDistanceThreshold) || (getMeters estDist.actualDistanceToPickup <= fromIntegral threshold)
        Nothing -> getMeters estDist.actualDistanceToPickup <= fromIntegral threshold

    makeDriverPoolRes QP.NearestGoHomeDriversResult {..} = DriverPoolResult {distanceToPickup = distanceToDriver, customerTags = Nothing, ..}

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
          staticDuration = Nothing,
          distance = Nothing,
          distanceWithUnit = Nothing,
          boundingBox = Nothing,
          snappedWaypoints = [],
          points = []
        }

    makeDriverPoolWithActualDistResult (driverPoolRes, _, ghrId, driverGoHomePoolWithActualDistance) = do
      DriverPoolWithActualDistResult
        { driverPoolResult = makeDriverPoolResult driverPoolRes,
          actualDistanceToPickup = driverGoHomePoolWithActualDistance.actualDistanceToPickup, --fromMaybe 0 driverRoute.distance,
          actualDurationToPickup = driverGoHomePoolWithActualDistance.actualDurationToPickup,
          intelligentScores = IntelligentScores Nothing Nothing Nothing Nothing Nothing Nothing transporterConfig.defaultPopupDelay,
          isPartOfIntelligentPool = False,
          pickupZone = False,
          specialZoneExtraTip = Nothing,
          searchTags = Nothing,
          tripDistance = Nothing,
          keepHiddenForSeconds = Seconds 0,
          goHomeReqId = if ghrId.getId == "specialLocWarriorGoHomeId" then Nothing else Just ghrId,
          isForwardRequest = False,
          previousDropGeoHash = Nothing,
          score = driverGoHomePoolWithActualDistance.score
        }

    makeDriverPoolResult QP.NearestGoHomeDriversResult {serviceTier = serviceTier', ..} =
      DriverPoolResult
        { distanceToPickup = distanceToDriver,
          serviceTier = serviceTier',
          customerTags = Nothing,
          ..
        }

data CalculateDriverPoolReq a = CalculateDriverPoolReq
  { cityServiceTiers :: [DVST.VehicleServiceTier],
    poolStage :: PoolCalculationStage,
    driverPoolCfg :: DriverPoolConfig,
    serviceTiers :: [DVST.ServiceTierType],
    pickup :: a,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    transporterConfig :: DTC.TransporterConfig,
    mRadiusStep :: Maybe PoolRadiusStep,
    isRental :: Bool,
    isInterCity :: Bool,
    isValueAddNP :: Bool,
    onlinePayment :: Bool,
    now :: UTCTime
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
  CalculateDriverPoolReq a ->
  m [DriverPoolResult]
calculateDriverPool CalculateDriverPoolReq {..} = do
  let radius = getRadius mRadiusStep
  let coord = getCoordinates pickup
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QPG.getNearestDrivers $
        QPG.NearestDriversReq
          { fromLocLatLong = coord,
            nearestRadius = radius,
            driverPositionInfoExpiry = driverPoolCfg.driverPositionInfoExpiry,
            ..
          }
  driversWithLessThanNParallelRequests <- case poolStage of
    DriverSelection -> filterM (fmap (< driverPoolCfg.maxParallelSearchRequests) . getParallelSearchRequestCount) approxDriverPool
    Estimate -> pure approxDriverPool --estimate stage we dont need to consider actual parallel request counts
  let driverPoolResult = makeDriverPoolResult <$> driversWithLessThanNParallelRequests
  logDebug $ "driverPoolResult: " <> show driverPoolResult
  logDebug $ "driverPoolResult: MetroWarriorDebugging-------" <> show driverPoolResult
  pure driverPoolResult
  where
    getParallelSearchRequestCount dObj = getValidSearchRequestCount merchantId (cast dObj.driverId) now
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
          customerTags = Nothing,
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
  CalculateDriverPoolReq a ->
  PoolType ->
  DST.CurrentSearchInfo ->
  m [DriverPoolWithActualDistResult]
calculateDriverPoolWithActualDist calculateReq@CalculateDriverPoolReq {..} poolType currentSearchInfo = do
  driverPool <- calculateDriverPool calculateReq
  case driverPool of
    [] -> return []
    (a : pprox) -> do
      filtDriverPoolWithActualDist' <-
        case poolType of
          SpecialZoneQueuePool -> pure $ map mkSpecialZoneQueueActualDistanceResult driverPool
          _ -> do
            driverPoolWithActualDist <- computeActualDistance driverPoolCfg.distanceUnit merchantId merchantOperatingCityId Nothing pickup (a :| pprox)
            pure $ case driverPoolCfg.actualDistanceThreshold of
              Nothing -> NE.toList driverPoolWithActualDist
              Just threshold -> map fst $ NE.filter (\(dis, dp) -> filterFunc threshold dis dp.distanceToPickup) $ NE.zip (NE.sortOn (.driverPoolResult.driverId) driverPoolWithActualDist) (NE.sortOn (.driverId) $ a :| pprox)
      logDebug $ "secondly filtered driver pool" <> show filtDriverPoolWithActualDist'
      filtDriverPoolWithActualDist <- filterM (scheduledRideFilter currentSearchInfo merchantId merchantOperatingCityId isRental isInterCity transporterConfig) filtDriverPoolWithActualDist'
      logDebug $ "thirdly scheduled filtered driver pool" <> show filtDriverPoolWithActualDist
      return filtDriverPoolWithActualDist
  where
    mkSpecialZoneQueueActualDistanceResult dpr = do
      DriverPoolWithActualDistResult
        { driverPoolResult = dpr,
          actualDistanceToPickup = dpr.distanceToPickup,
          actualDurationToPickup = Seconds 180, -- deafult 3 minutes here as its a queue on the gate
          intelligentScores = IntelligentScores Nothing Nothing Nothing Nothing Nothing Nothing 0,
          isPartOfIntelligentPool = False,
          pickupZone = False,
          specialZoneExtraTip = Nothing,
          searchTags = Nothing,
          tripDistance = Nothing,
          keepHiddenForSeconds = Seconds 0,
          goHomeReqId = Nothing,
          isForwardRequest = False,
          previousDropGeoHash = Nothing,
          score = dpr.score
        }

    filterFunc threshold estDist distanceToPickup =
      case driverPoolCfg.thresholdToIgnoreActualDistanceThreshold of
        Just thresholdToIgnoreActualDistanceThreshold -> (distanceToPickup <= thresholdToIgnoreActualDistanceThreshold) || (getMeters estDist.actualDistanceToPickup <= fromIntegral threshold)
        Nothing -> getMeters estDist.actualDistanceToPickup <= fromIntegral threshold

scheduledRideFilter :: (MonadFlow m, MonadTime m, LT.HasLocationService m r, ServiceFlow m r) => DST.CurrentSearchInfo -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Bool -> DTC.TransporterConfig -> DriverPoolWithActualDistResult -> m Bool
scheduledRideFilter currentSearchInfo merchantId merchantOpCityId isRental isIntercity transporterConfig driverPoolWithActualDistResult = do
  now <- getCurrentTime
  let driverInfo = driverPoolWithActualDistResult.driverPoolResult
  let minimumScheduledBookingLeadTimeInSecs = KP.intToNominalDiffTime (transporterConfig.minmRentalAndScheduledBookingLeadTimeHours.getHours * 3600)
      scheduledRideFilterExclusionThresholdInSecs = KP.intToNominalDiffTime (transporterConfig.scheduledRideFilterExclusionThresholdHours.getHours * 3600)
      haveScheduled = isJust driverInfo.latestScheduledBooking
  if
      | haveScheduled && isIntercity -> return False
      | haveScheduled && isRental -> return $ canTakeRental driverInfo.latestScheduledBooking now minimumScheduledBookingLeadTimeInSecs
      | isScheduledRideUnderFilterExclusionThresholdHours driverInfo.latestScheduledBooking now scheduledRideFilterExclusionThresholdInSecs -> do
        case (currentSearchInfo.dropLocation, driverInfo.latestScheduledPickup, currentSearchInfo.routeDistance, transporterConfig.avgSpeedOfVehicle) of
          (Just dropLoc, Just scheduledPickup, Just routeDistance, Just avgSpeeds) -> do
            currentDroptoScheduledPickupDistance <-
              TMaps.getDistanceForScheduledRides merchantId merchantOpCityId $
                TMaps.GetDistanceReq
                  { origin = dropLoc,
                    destination = scheduledPickup,
                    travelMode = Just TMaps.CAR,
                    sourceDestinationMapping = Nothing,
                    distanceUnit = Meter
                  }
            let avgSpeedOfVehicleInKM = getVehicleAvgSpeed driverInfo.variant avgSpeeds
                destToPickupDistance = currentDroptoScheduledPickupDistance.distance
                totalDistanceinM = routeDistance + destToPickupDistance + driverPoolWithActualDistResult.actualDistanceToPickup
                totalDistanceinKM = (fromIntegral (totalDistanceinM.getMeters) :: Double) / 1000
                totalTimeinDoubleHr = (totalDistanceinKM / fromIntegral (avgSpeedOfVehicleInKM.getKilometers)) :: Double
                totalTimeInSeconds = realToFrac (totalTimeinDoubleHr * 3600) :: NominalDiffTime
                expectedEndTime = addUTCTime totalTimeInSeconds now
            let isRidePossible = case driverInfo.latestScheduledBooking of
                  Just latestScheduledBooking ->
                    let timeDifference = diffUTCTime latestScheduledBooking (addUTCTime transporterConfig.scheduleRideBufferTime expectedEndTime)
                     in timeDifference > 0
                  Nothing -> False
            return isRidePossible
          (_, _, _, _) -> return False
      | otherwise -> return True
  where
    canTakeRental :: Maybe UTCTime -> UTCTime -> NominalDiffTime -> Bool
    canTakeRental mbLatestScheduledBooking now minimumScheduledBookingLeadTimeInSecs =
      case mbLatestScheduledBooking of
        Nothing -> True
        Just latestScheduledBooking ->
          let timeDifference = diffUTCTime latestScheduledBooking now
           in timeDifference >= minimumScheduledBookingLeadTimeInSecs
    isScheduledRideUnderFilterExclusionThresholdHours :: Maybe UTCTime -> UTCTime -> NominalDiffTime -> Bool
    isScheduledRideUnderFilterExclusionThresholdHours mbLatestScheduledBooking now scheduledRideFilterExclusionThresholdInSecs =
      case mbLatestScheduledBooking of
        Nothing -> False
        Just latestScheduledBooking ->
          let timeDifference = diffUTCTime latestScheduledBooking now
           in timeDifference < scheduledRideFilterExclusionThresholdInSecs

getVehicleAvgSpeed :: DVeh.VehicleVariant -> DTC.AvgSpeedOfVechilePerKm -> Kilometers
getVehicleAvgSpeed variant avgSpeedOfVehicle = case variant of
  DVeh.SEDAN -> avgSpeedOfVehicle.sedan
  DVeh.SUV -> avgSpeedOfVehicle.suv
  DVeh.HATCHBACK -> avgSpeedOfVehicle.hatchback
  DVeh.AUTO_RICKSHAW -> avgSpeedOfVehicle.autorickshaw
  DVeh.TAXI -> avgSpeedOfVehicle.taxi
  DVeh.TAXI_PLUS -> avgSpeedOfVehicle.taxiplus
  DVeh.PREMIUM_SEDAN -> avgSpeedOfVehicle.premiumsedan
  DVeh.BLACK -> avgSpeedOfVehicle.black
  DVeh.BLACK_XL -> avgSpeedOfVehicle.blackxl
  DVeh.BIKE -> avgSpeedOfVehicle.bike
  DVeh.DELIVERY_BIKE -> avgSpeedOfVehicle.bike
  DVeh.AMBULANCE_TAXI -> avgSpeedOfVehicle.ambulance
  DVeh.AMBULANCE_TAXI_OXY -> avgSpeedOfVehicle.ambulance
  DVeh.AMBULANCE_AC -> avgSpeedOfVehicle.ambulance
  DVeh.AMBULANCE_AC_OXY -> avgSpeedOfVehicle.ambulance
  DVeh.AMBULANCE_VENTILATOR -> avgSpeedOfVehicle.ambulance
  DVeh.SUV_PLUS -> avgSpeedOfVehicle.suvplus
  DVeh.HERITAGE_CAB -> avgSpeedOfVehicle.heritagecab
  DVeh.EV_AUTO_RICKSHAW -> avgSpeedOfVehicle.evautorickshaw
  DVeh.DELIVERY_LIGHT_GOODS_VEHICLE -> avgSpeedOfVehicle.deliveryLightGoodsVehicle
  DVeh.DELIVERY_TRUCK_MINI -> avgSpeedOfVehicle.deliveryLightGoodsVehicle
  DVeh.DELIVERY_TRUCK_SMALL -> avgSpeedOfVehicle.deliveryLightGoodsVehicle
  DVeh.DELIVERY_TRUCK_MEDIUM -> avgSpeedOfVehicle.deliveryLightGoodsVehicle
  DVeh.DELIVERY_TRUCK_LARGE -> avgSpeedOfVehicle.deliveryLightGoodsVehicle
  DVeh.DELIVERY_TRUCK_ULTRA_LARGE -> avgSpeedOfVehicle.deliveryLightGoodsVehicle
  DVeh.BUS_NON_AC -> avgSpeedOfVehicle.busNonAc
  DVeh.BUS_AC -> avgSpeedOfVehicle.busAc

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
  CalculateDriverPoolReq a ->
  Maybe Integer ->
  m (Meters, [DriverPoolResultCurrentlyOnRide])
calculateDriverPoolCurrentlyOnRide CalculateDriverPoolReq {..} mbBatchNum = do
  let radius = getRadius' mRadiusStep
  let coord = getCoordinates pickup
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPoolCurrentlyOnRide" $
      B.runInReplica $
        QP.getNearestDriversCurrentlyOnRide $
          QP.NearestDriversOnRideReq
            { fromLocLatLong = coord,
              nearestRadius = radius,
              driverPositionInfoExpiry = driverPoolCfg.driverPositionInfoExpiry,
              currentRideTripCategoryValidForForwardBatching = driverPoolCfg.currentRideTripCategoryValidForForwardBatching,
              ..
            }
  driversWithLessThanNParallelRequests <- case poolStage of
    DriverSelection -> filterM (fmap (< driverPoolCfg.maxParallelSearchRequestsOnRide) . getParallelSearchRequestCount) approxDriverPool
    Estimate -> pure approxDriverPool --estimate stage we dont need to consider actual parallel request counts
  pure (radius, makeDriverPoolResult <$> driversWithLessThanNParallelRequests)
  where
    getParallelSearchRequestCount dObj = getValidSearchRequestCount merchantId (cast dObj.driverId) now
    getRadius' mRadiusStep_ = do
      let radiusParams = driverPoolCfg.onRideRadiusConfig
      let maxRadiusThreshold = getRadius mRadiusStep_
      case (radiusParams, mbBatchNum) of
        ([], _) -> maxRadiusThreshold
        (_ : _, Nothing) -> maxRadiusThreshold
        (_ : _, Just batchNum) -> getMaxRadiusWithThreshold radiusParams maxRadiusThreshold batchNum
    getMaxRadiusWithThreshold radiusList threshold batchNum = do
      let mbRadius = find (\r -> r.batchNumber == fromIntegral batchNum) radiusList
      maybe threshold (\r -> max threshold r.onRideRadius) mbRadius
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
  CalculateDriverPoolReq a ->
  PoolType ->
  Integer ->
  DST.CurrentSearchInfo ->
  m [DriverPoolWithActualDistResult]
calculateDriverCurrentlyOnRideWithActualDist calculateReq@CalculateDriverPoolReq {..} poolType batchNum currentSearchInfo = do
  (thresholdRadius, driverPoolStraightLineFiltered) <- calculateDriverPoolCurrentlyOnRide calculateReq (Just batchNum)
  let countDriversToProccess = fromMaybe 10 driverPoolCfg.batchSizeOnRideWithStraightLineDistance
  let driverPool = take countDriversToProccess $ sortOn (.distanceToPickup) driverPoolStraightLineFiltered
  logDebug $ "driverPoolcalculateDriverCurrentlyOnRideWithActualDist" <> show driverPool
  case driverPool of
    [] -> do
      logDebug "driverPool is empty"
      return []
    (a : pprox) -> do
      let driverPoolResultsWithDriverLocationAsDestinationLocation = driverResultFromDestinationLocation <$> (a :| pprox)
          driverToDestinationDistanceThreshold = driverPoolCfg.driverToDestinationDistanceThreshold
      driverPoolWithActualDistFromDestinationLocation <- computeActualDistance driverPoolCfg.distanceUnit merchantId merchantOperatingCityId Nothing pickup driverPoolResultsWithDriverLocationAsDestinationLocation
      driverPoolWithActualDistFromCurrentLocation <- do
        case driverPoolCfg.useOneToOneOsrmMapping of
          Just True -> calculateActualDistanceCurrentlyOneToOneSrcAndDestMapping (a :| pprox)
          _ -> traverse (calculateActualDistanceCurrently driverToDestinationDistanceThreshold) (a :| pprox)
      let driverPoolWithActualDist = catMaybes $ zipWith (curry $ combine driverToDestinationDistanceThreshold) (NE.toList driverPoolWithActualDistFromDestinationLocation) (NE.toList driverPoolWithActualDistFromCurrentLocation)
          filtDriverPoolWithActualDist' = case (driverPoolCfg.actualDistanceThresholdOnRide, poolType) of
            (_, SpecialZoneQueuePool) -> driverPoolWithActualDist
            (Nothing, _) -> filter (filterFunc thresholdRadius) driverPoolWithActualDist
            (Just threshold, _) -> filter (filterFunc threshold) driverPoolWithActualDist
      logDebug $ "secondly filtered driver pool" <> show filtDriverPoolWithActualDist'
      logDebug $ "driverPoolWithActualDist" <> show driverPoolWithActualDist
      filtDriverPoolWithActualDist <- filterM (scheduledRideFilter currentSearchInfo merchantId merchantOperatingCityId isRental isInterCity transporterConfig) filtDriverPoolWithActualDist'
      logDebug $ "thirdly scheduled filtered driver pool" <> show filtDriverPoolWithActualDist
      return filtDriverPoolWithActualDist
  where
    filterFunc threshold estDist = getMeters estDist.actualDistanceToPickup <= fromIntegral threshold

    driverResultFromDestinationLocation DriverPoolResultCurrentlyOnRide {..} =
      DriverPoolResult
        { lat = previousRideDropLat,
          lon = previousRideDropLon,
          customerTags = Nothing,
          ..
        }
    calculateActualDistanceCurrently _driverToDestinationDistanceThreshold DriverPoolResultCurrentlyOnRide {..} = do
      let temp = DriverPoolResult {customerTags = Nothing, ..}
      computeActualDistanceOneToOne driverPoolCfg.distanceUnit merchantId merchantOperatingCityId (Just $ LatLong previousRideDropLat previousRideDropLon) (LatLong previousRideDropLat previousRideDropLon) temp
    combine driverToDestinationDistanceThreshold (DriverPoolWithActualDistResult {actualDistanceToPickup = x, actualDurationToPickup = y, previousDropGeoHash = pDGeoHash}, DriverPoolWithActualDistResult {..}) =
      if actualDistanceToPickup < driverToDestinationDistanceThreshold
        then
          Just
            DriverPoolWithActualDistResult
              { actualDistanceToPickup = x + actualDistanceToPickup,
                actualDurationToPickup = y + actualDurationToPickup,
                isForwardRequest = True,
                previousDropGeoHash = pDGeoHash <|> previousDropGeoHash,
                ..
              }
        else Nothing
    calculateActualDistanceCurrentlyOneToOneSrcAndDestMapping driverPoolCurrentlyOnRide = do
      let driverPoolResultsWithDriverLocationAsCurrentLocation = map (\DriverPoolResultCurrentlyOnRide {..} -> DriverPoolResult {customerTags = Nothing, ..}) driverPoolCurrentlyOnRide
      let mbPreviousRideDropLatLn = NE.toList $ map (\DriverPoolResultCurrentlyOnRide {..} -> Just $ LatLong previousRideDropLat previousRideDropLon) driverPoolCurrentlyOnRide
      let previousRideDropLatLn = NE.fromList $ catMaybes mbPreviousRideDropLatLn
      computeActualDistanceOneToOneSrcAndDestMapping driverPoolCfg.distanceUnit merchantId merchantOperatingCityId previousRideDropLatLn mbPreviousRideDropLatLn driverPoolResultsWithDriverLocationAsCurrentLocation

computeActualDistanceOneToOne ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasCoordinates a
  ) =>
  DistanceUnit ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe LatLong ->
  a ->
  DriverPoolResult ->
  m DriverPoolWithActualDistResult
computeActualDistanceOneToOne distanceUnit merchantId merchantOpCityId prevRideDropLatLn pickup driverPoolResult = do
  (ele :| _) <- computeActualDistance distanceUnit merchantId merchantOpCityId prevRideDropLatLn pickup (driverPoolResult :| [])
  pure ele

computeActualDistance ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasCoordinates a
  ) =>
  DistanceUnit ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe LatLong ->
  a ->
  NonEmpty DriverPoolResult ->
  m (NonEmpty DriverPoolWithActualDistResult)
computeActualDistance distanceUnit orgId merchantOpCityId prevRideDropLatLn pickup driverPoolResults = do
  let pickupLatLong = getCoordinates pickup
  transporter <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId.getId)
  getDistanceResults <-
    Maps.getEstimatedPickupDistances orgId merchantOpCityId $
      Maps.GetDistancesReq
        { origins = driverPoolResults,
          destinations = pickupLatLong :| [],
          travelMode = Just Maps.CAR,
          sourceDestinationMapping = Nothing,
          distanceUnit
        }
  logDebug $ "get distance results" <> show getDistanceResults
  prevRideDropGeoHash <- case prevRideDropLatLn of
    Just (LatLong lat lon) -> pure $ T.pack <$> DG.encode 9 (lat, lon)
    Nothing -> pure Nothing
  return $ mkDriverPoolWithActualDistResult transporter.defaultPopupDelay prevRideDropGeoHash <$> getDistanceResults
  where
    mkDriverPoolWithActualDistResult defaultPopupDelay prevRideDropGeoHash distDur = do
      DriverPoolWithActualDistResult
        { driverPoolResult = distDur.origin,
          actualDistanceToPickup = distDur.distance,
          actualDurationToPickup = distDur.duration,
          intelligentScores = IntelligentScores Nothing Nothing Nothing Nothing Nothing Nothing defaultPopupDelay,
          isPartOfIntelligentPool = False,
          pickupZone = False,
          specialZoneExtraTip = Nothing,
          searchTags = Nothing,
          tripDistance = Nothing,
          keepHiddenForSeconds = Seconds 0,
          goHomeReqId = Nothing,
          isForwardRequest = False,
          previousDropGeoHash = prevRideDropGeoHash,
          score = distDur.origin.score
        }

computeActualDistanceOneToOneSrcAndDestMapping ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  DistanceUnit ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  NonEmpty LatLong ->
  [Maybe LatLong] ->
  NonEmpty DriverPoolResult ->
  m (NonEmpty DriverPoolWithActualDistResult)
computeActualDistanceOneToOneSrcAndDestMapping distanceUnit orgId merchantOpCityId destinationLatLons previousDropPoints driverPoolResults = do
  transporter <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId.getId)
  getDistanceResults <-
    Maps.getEstimatedPickupDistances orgId merchantOpCityId $
      Maps.GetDistancesReq
        { origins = driverPoolResults,
          destinations = destinationLatLons,
          travelMode = Just Maps.CAR,
          sourceDestinationMapping = Just Maps.OneToOne,
          distanceUnit
        }
  logDebug $ "get distance results one to one mapping" <> show getDistanceResults
  let distanceAndDropPointsZipped = zip previousDropPoints (NE.toList getDistanceResults)
  driverPoolEntities <-
    mapM
      ( \(prevRideDropLatLn, getDistanceResult) -> do
          prevRideDropGeoHash <- case prevRideDropLatLn of
            Just (LatLong lat lon) -> pure $ T.pack <$> DG.encode 9 (lat, lon)
            Nothing -> pure Nothing
          return $ mkDriverPoolWithActualDistResult transporter.defaultPopupDelay prevRideDropGeoHash getDistanceResult
      )
      distanceAndDropPointsZipped
  return $ NE.fromList driverPoolEntities
  where
    mkDriverPoolWithActualDistResult defaultPopupDelay prevRideDropGeoHash distDur = do
      DriverPoolWithActualDistResult
        { driverPoolResult = distDur.origin,
          actualDistanceToPickup = distDur.distance,
          actualDurationToPickup = distDur.duration,
          intelligentScores = IntelligentScores Nothing Nothing Nothing Nothing Nothing Nothing defaultPopupDelay,
          isPartOfIntelligentPool = False,
          pickupZone = False,
          specialZoneExtraTip = Nothing,
          searchTags = Nothing,
          tripDistance = Nothing,
          keepHiddenForSeconds = Seconds 0,
          goHomeReqId = Nothing,
          isForwardRequest = False,
          previousDropGeoHash = prevRideDropGeoHash,
          score = distDur.origin.score
        }

refactorRoutesResp :: GoHomeConfig -> (QP.NearestGoHomeDriversResult, Maps.RouteInfo, Id DDGR.DriverGoHomeRequest, DriverPoolWithActualDistResult) -> (QP.NearestGoHomeDriversResult, Maps.RouteInfo, Id DDGR.DriverGoHomeRequest, DriverPoolWithActualDistResult)
refactorRoutesResp goHomeCfg (nearestDriverRes, route, ghrId, driverGoHomePoolWithActualDistance) = (nearestDriverRes, newRoute route, ghrId, driverGoHomePoolWithActualDistance)
  where
    newRoute route' =
      RouteInfo
        { distance = route'.distance,
          distanceWithUnit = route'.distanceWithUnit,
          duration = route'.duration,
          staticDuration = route'.staticDuration,
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
