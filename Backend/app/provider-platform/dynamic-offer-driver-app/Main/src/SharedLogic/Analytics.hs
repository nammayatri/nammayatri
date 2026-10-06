{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Analytics where

import qualified Crypto.Hash as Hash
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.DriverFlowStatus as DDF
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SubscriptionPurchase as DSP
import qualified Domain.Types.TransporterConfig as TC
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.AnalyticsExtra
import qualified SharedLogic.DriverFlowStatus as SDFStatus
import SharedLogic.FleetAnalytics.Realtime
import qualified SharedLogic.FleetOperatorStats as SFleetOperatorStats
import qualified Storage.Clickhouse.DriverInformation as CDI
import qualified Storage.Clickhouse.FleetOperatorDailyStats as CFleetOpDailyStats
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOperatorDailyStatsExtra as QFleetOpsDailyExtra
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubscriptionPurchaseExtra
import Tools.Error hiding (CustomerCancelled)

-- | Update analytics and driver stats counters for a cancelled ride.
updateCancellationAnalyticsAndDriverStats ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    MonadReader r m,
    HasKafkaProducer r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  DRide.Ride ->
  SBCR.CancellationSource ->
  m ()
updateCancellationAnalyticsAndDriverStats transporterConfig ride source = do
  driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  case source of
    SBCR.ByDriver -> do
      recordFleetOperatorAnalyticsImpl transporterConfig (DriverCancelled ride.driverId ride.id.getId)
      QDriverStats.updateValidDriverCancellationTagCount (driverStats.validDriverCancellationTagCount + 1) ride.driverId
    SBCR.ByUser -> do
      recordFleetOperatorAnalyticsImpl transporterConfig (CustomerCancelled ride.driverId ride.id.getId)
      QDriverStats.updateValidCustomerCancellationTagCount (driverStats.validCustomerCancellationTagCount + 1) ride.driverId
    _ -> pure ()

-- | Update fleet owner analytics counters in Redis. Passing Nothing deletes the key.
updateFleetOwnerAnalyticsKeys ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  m ()
updateFleetOwnerAnalyticsKeys fleetOwnerId mbActiveDrivers mbActiveVehicles mbCurrentOnline = do
  let setOrDel key = \case
        Just v -> Redis.set key v
        Nothing -> Redis.del key >> pure ()

  -- active driver count
  let adcKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_DRIVER_COUNT
  setOrDel adcKey mbActiveDrivers

  -- active vehicle count
  let avcKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_VEHICLE_COUNT
  setOrDel avcKey mbActiveVehicles

  -- current online driver count uses ONLINE status key
  let codKey = DDF.getStatusKey fleetOwnerId DDF.ONLINE
  setOrDel codKey mbCurrentOnline

-- | Helper function to extract fleet analytics data from common result
extractFleetAnalyticsData :: CommonAllTimeFallbackRes -> Flow (Maybe Int, Maybe Int, Maybe Int)
extractFleetAnalyticsData (FleetAllTimeFallback fleetData) = pure (fleetData.activeDriverCount, fleetData.activeVehicleCount, fleetData.currentOnlineDriverCount)
extractFleetAnalyticsData _ = throwError $ InvalidRequest "Expected FleetAllTimeFallback but got OperatorAllTimeFallback"

extractOperatorAnalyticsData :: CommonAllTimeFallbackRes -> Flow (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int)
extractOperatorAnalyticsData (OperatorAllTimeFallback operatorData) =
  pure
    ( operatorData.totalRideCount,
      operatorData.ratingSum,
      operatorData.ratingCount,
      operatorData.cancelCount,
      operatorData.acceptationCount,
      operatorData.totalRequestCount,
      operatorData.totalAssociatedDriver,
      operatorData.totalActiveDrivers,
      operatorData.totalEnabledDrivers
    )
extractOperatorAnalyticsData _ = throwError $ InvalidRequest "Expected OperatorAllTimeFallback but got FleetAllTimeFallback"

data PeriodRes = PeriodRes
  { activeDriver :: Int,
    driverEnabled :: Int,
    greaterThanOneRide :: Int,
    greaterThanTenRide :: Int,
    greaterThanFiftyRide :: Int,
    approvedDriverInspection :: Int,
    approvedVehicleInspection :: Int,
    rejectedDriverInspection :: Int,
    rejectedVehicleInspection :: Int
  }
  deriving (Show, Eq)

-- | One fact per thing that happened. Postgres columns and the operator Redis
-- message for that fact live in the same case, so a new metric cannot be added
-- to one path and forgotten on the other. With the consumer flag on, the fact
-- itself is the Kafka payload.
data FleetOperatorAnalytics
  = RideCompleted (Id DP.Person) SFleetOperatorStats.CompletedRideStats
  | DriverCancelled (Id DP.Person) Text
  | CustomerCancelled (Id DP.Person) Text
  | -- Drivers, search try id, and searchBatchKey of the dispatch. A search try sends
    -- several batches, and top-ups reuse the batch number, so the try id alone is not unique.
    SearchRequested [Id DP.Person] Text Text
  | OfferAccepted (Id DP.Person) Text
  | OfferRejected (Id DP.Person) Text
  | OfferPulled (Id DP.Person) Text
  | RatingSubmitted (Id DP.Person) Int Bool Text Int
  deriving (Generic, Show, ToJSON, FromJSON)

class Monad m => PublishesFleetAnalytics m where
  recordFleetOperatorAnalytics :: TC.TransporterConfig -> FleetOperatorAnalytics -> m ()

-- Any request monad with these capabilities, so the driver app, the allocator and the
-- kafka consumer share one instance.
instance
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    MonadReader r m,
    HasKafkaProducer r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  PublishesFleetAnalytics m
  where
  recordFleetOperatorAnalytics = recordFleetOperatorAnalyticsImpl

recordFleetOperatorAnalyticsImpl ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    MonadReader r m,
    HasKafkaProducer r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  FleetOperatorAnalytics ->
  m ()
recordFleetOperatorAnalyticsImpl transporterConfig fact =
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
    let viaConsumer = transporterConfig.analyticsConfig.fleetAnalyticsRedisViaConsumer
    if viaConsumer
      then do
        published <- publishFleetRealtimeEvent transporterConfig (fleetEventId fact) fact
        -- A failed publish never reached the topic, so the request applies it. A success is left
        -- to the consumer; applying here as well would count the event twice.
        unless published $ applyFleetOperatorAnalytics transporterConfig (\_ postgresWrite redisWrite -> postgresWrite >> redisWrite) fact
      else applyFleetOperatorAnalytics transporterConfig (\_ postgresWrite redisWrite -> postgresWrite >> redisWrite) fact

-- | Postgres increments, and for the operator the Redis counters in the same lock hold. Used on
-- the request when the consumer flag is off or the publish failed, and by the consumer, which
-- passes a per-entity claim as claimEntity. claimEntity runs the Postgres writes, then the Redis
-- counters, inside the entity lock.
applyFleetOperatorAnalytics ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  (Text -> m () -> m () -> m ()) ->
  FleetOperatorAnalytics ->
  m ()
applyFleetOperatorAnalytics transporterConfig claimEntity fact =
  case fact of
    CustomerCancelled driverId _rideId ->
      forOperatorAndFleet "AnalyticsUpdateCustomerCancelCount" driverId [] $ \entityId -> do
        SFleetOperatorStats.incrementCustomerCancellationCount entityId transporterConfig
        SFleetOperatorStats.incrementCustomerCancellationCountDaily entityId driverId.getId transporterConfig
    OfferRejected driverId _searchTryId ->
      forOperatorAndFleet "AnalyticsUpdateAcceptationAndTotalRequestCount" driverId [] $ \entityId ->
        SFleetOperatorStats.incrementRequestCountsDaily entityId driverId.getId transporterConfig False False True False
    OfferPulled driverId _searchTryId ->
      forOperatorAndFleet "AnalyticsUpdateAcceptationAndTotalRequestCount" driverId [] $ \entityId ->
        SFleetOperatorStats.incrementRequestCountsDaily entityId driverId.getId transporterConfig False False False True
    DriverCancelled driverId _rideId ->
      forOperatorAndFleet "AnalyticsUpdateCancelCount" driverId [(CANCEL_COUNT, 1)] $ \entityId -> do
        SFleetOperatorStats.incrementDriverCancellationCount entityId transporterConfig
        SFleetOperatorStats.incrementDriverCancellationCountDaily entityId driverId.getId transporterConfig
    OfferAccepted driverId _searchTryId ->
      forOperatorAndFleet "AnalyticsUpdateAcceptationAndTotalRequestCount" driverId [(ACCEPTATION_COUNT, 1)] $ \entityId -> do
        SFleetOperatorStats.incrementRequestCounts entityId transporterConfig True False
        SFleetOperatorStats.incrementRequestCountsDaily entityId driverId.getId transporterConfig True False False False
    SearchRequested driverIds _searchTryId _batchKey -> do
      operatorAssociations <- QDOA.findAllByDriverIds driverIds
      let operatorDriverPairs = [(oa.operatorId, oa.driverId.getId) | oa <- operatorAssociations]
      unless (null operatorDriverPairs) $
        SFleetOperatorStats.incrementTotalRequestCountBatch operatorDriverPairs transporterConfig claimEntity $ \operatorId driverCount ->
          incrOperatorCounter transporterConfig operatorId TOTAL_REQUEST_COUNT (fromIntegral driverCount)
      fleetAssociations <- QFDA.findAllByDriverIds driverIds
      let fleetDriverPairs = [(fa.fleetOwnerId, fa.driverId.getId) | fa <- fleetAssociations]
      unless (null fleetDriverPairs) $
        SFleetOperatorStats.incrementTotalRequestCountBatch fleetDriverPairs transporterConfig claimEntity (\_ _ -> pure ())
    RatingSubmitted driverId ratingValue shouldIncrementCount _rideId _totalRatingCount ->
      forOperatorAndFleet
        "AnalyticsUpdateRatingScoreKey"
        driverId
        [(RATING_SUM, fromIntegral ratingValue), (RATING_COUNT, if shouldIncrementCount then 1 else 0)]
        $ \entityId -> do
          SFleetOperatorStats.incrementTotalRatingCountAndTotalRatingScore entityId transporterConfig ratingValue shouldIncrementCount
          SFleetOperatorStats.incrementTotalRatingCountAndTotalRatingScoreDaily entityId driverId.getId transporterConfig ratingValue shouldIncrementCount
    RideCompleted driverId rideStats ->
      forOperatorAndFleet "AnalyticsUpdateTotalRideCount" driverId [(TOTAL_RIDE_COUNT, 1)] $ \entityId -> do
        SFleetOperatorStats.incrementTotalRidesTotalDistAndTotalEarning entityId rideStats transporterConfig
        SFleetOperatorStats.incrementTotalEarningDistanceAndCompletedRidesDaily entityId rideStats transporterConfig
  where
    forOperatorAndFleet tag driverId operatorCounters step = do
      operatorIds <- findOperatorIdForDriver driverId
      when (null operatorIds) $ logTagInfo tag $ "No operator found for driver: " <> show driverId
      forM_ operatorIds $ \operatorId ->
        Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId) 10 5000 $
          claimEntity
            operatorId
            (step operatorId)
            (forM_ operatorCounters $ \(metric, amount) -> incrOperatorCounter transporterConfig operatorId metric amount)
      mbFleetOwner <- QFDA.findByDriverId driverId True
      when (isNothing mbFleetOwner) $ logTagInfo tag $ "No fleet owner found for driver: " <> show driverId
      whenJust mbFleetOwner $ \fleetOwner ->
        Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId) 10 5000 $
          claimEntity fleetOwner.fleetOwnerId (step fleetOwner.fleetOwnerId) (pure ())

applyPublishedFleetAnalytics ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  FleetRealtimeEvent FleetOperatorAnalytics ->
  m ()
applyPublishedFleetAnalytics transporterConfig event =
  applyFleetOperatorAnalytics transporterConfig (withFleetEventClaim event.eventId) event.payload

-- | Each dispatch creates new SearchRequestForDriver rows, so their ids identify one batch
-- (top-ups included), and a redelivered event carries the same key.
searchBatchKey :: [Text] -> Text
searchBatchKey searchRequestForDriverIds =
  show $ Hash.hashWith Hash.SHA256 (TE.encodeUtf8 (T.intercalate "," (sort searchRequestForDriverIds)))

fleetEventId :: FleetOperatorAnalytics -> Text
fleetEventId = \case
  RideCompleted _ rideStats -> "ride.completed:" <> rideStats.rideId
  DriverCancelled _ rideId -> "ride.cancelled:" <> rideId
  CustomerCancelled _ rideId -> "ride.customerCancelled:" <> rideId
  SearchRequested _ searchTryId batchKey -> "search.requested:" <> searchTryId <> ":" <> batchKey
  OfferAccepted driverId searchTryId -> "quote.responded:" <> searchTryId <> ":" <> driverId.getId <> ":Accept"
  OfferRejected driverId searchTryId -> "quote.responded:" <> searchTryId <> ":" <> driverId.getId <> ":Reject"
  OfferPulled driverId searchTryId -> "quote.responded:" <> searchTryId <> ":" <> driverId.getId <> ":Pull"
  RatingSubmitted _ ratingValue _ rideId totalRatingCount ->
    "rating.submitted:" <> rideId <> ":" <> show totalRatingCount <> ":" <> show ratingValue

-- case newTotalRides of
--   2 -> updatePeriodicMetrics transporterConfig operatorId GREATER_THAN_ONE_RIDE Redis.incr
--   11 -> updatePeriodicMetrics transporterConfig operatorId GREATER_THAN_TEN_RIDE Redis.incr
--   51 -> updatePeriodicMetrics transporterConfig operatorId GREATER_THAN_FIFTY_RIDE Redis.incr
--   _ -> pure ()

updateEnabledVerifiedStateWithAnalytics :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, HasField "serviceClickhouseCfg" r CH.ClickhouseCfg, HasField "serviceClickhouseEnv" r CH.ClickhouseEnv) => Maybe DI.DriverInformation -> TC.TransporterConfig -> Id DP.Person -> Bool -> Maybe Bool -> m ()
updateEnabledVerifiedStateWithAnalytics mbDriverInfoData transporterConfig driverId isEnabled isVerified = do
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
    mbDriverInfo <- case mbDriverInfoData of
      Just driverInfoData -> pure (Just driverInfoData)
      Nothing -> QDI.findById driverId
    when (isNothing mbDriverInfo) $ logTagError "AnalyticsUpdateEnabledVerifiedState" $ "No driver info found for driver: " <> show driverId
    whenJust mbDriverInfo $ \di ->
      when (di.enabled /= isEnabled) $ do
        operatorIds <- findOperatorIdForDriver driverId
        let delta = if isEnabled then 1 :: Integer else -1
        forM_ operatorIds $ \oid ->
          adjustOperatorAllTimeAnalyticsMetric transporterConfig oid TOTAL_ENABLED_DRIVERS delta
  -- BOT: setting enabled or verified to False also revokes `approved` (re-approval required).
  let isApproved = if transporterConfig.enableBotFlow == Just True && (not isEnabled || isVerified == Just False) then Just False else Nothing
  QDI.updateEnabledVerifiedState driverId isEnabled isVerified isApproved

incrementOperatorTotalActiveDriversIfFirstDriverSubscription ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  m ()
incrementOperatorTotalActiveDriversIfFirstDriverSubscription transporterConfig driverOwnerIdText =
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
    operatorIds <- findOperatorIdForDriver (Id driverOwnerIdText)
    unless (null operatorIds) $ do
      activeCount <- QSubscriptionPurchaseExtra.countActiveSubscriptionsForOwner driverOwnerIdText DSP.DRIVER
      when (activeCount == 0) $
        forM_ operatorIds $ \oid ->
          adjustOperatorAllTimeAnalyticsMetric transporterConfig oid TOTAL_ACTIVE_DRIVERS 1

-- Updation Logic of Fleet Owner Fields
incrementFleetOwnerAnalyticsActiveDriverCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Maybe Text ->
  Id DP.Person ->
  m ()
incrementFleetOwnerAnalyticsActiveDriverCount transporterConfig mbFleetOwnerId driverId = do
  case mbFleetOwnerId of
    Just fleetOwnerId -> incrementActiveDriverCount fleetOwnerId
    Nothing -> do
      mbFleetOwner <- QFDA.findByDriverId driverId True
      when (isNothing mbFleetOwner) $ logTagError "AnalyticsUpdateActiveDriverCount" $ "No fleet owner found for driver: " <> show driverId
      whenJust mbFleetOwner $ \fleetOwner -> incrementActiveDriverCount fleetOwner.fleetOwnerId
  where
    incrementActiveDriverCount fleetOwnerId = do
      let totalActiveDriverCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_DRIVER_COUNT
      ensureRedisKeysExistForAllTimeCommon transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveDriverCountKey Redis.incrby 1

decrementFleetOwnerAnalyticsActiveDriverCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Maybe Text ->
  Id DP.Person ->
  m ()
decrementFleetOwnerAnalyticsActiveDriverCount transporterConfig mbFleetOwnerId driverId = do
  case mbFleetOwnerId of
    Just fleetOwnerId -> decrementActiveDriverCount fleetOwnerId
    Nothing -> do
      mbFleetOwner <- QFDA.findByDriverId driverId True
      when (isNothing mbFleetOwner) $ logTagError "AnalyticsUpdateActiveDriverCount" $ "No fleet owner found for driver: " <> show driverId
      whenJust mbFleetOwner $ \fleetOwner -> decrementActiveDriverCount fleetOwner.fleetOwnerId
  where
    decrementActiveDriverCount fleetOwnerId = do
      let totalActiveDriverCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_DRIVER_COUNT
      ensureRedisKeysExistForAllTimeCommon transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveDriverCountKey Redis.decrby 1

incrementFleetOwnerAnalyticsActiveVehicleCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Maybe Text ->
  Id DP.Person ->
  m ()
incrementFleetOwnerAnalyticsActiveVehicleCount transporterConfig mbFleetOwnerId driverId = do
  when (isNothing mbFleetOwnerId) $ logTagError "AnalyticsUpdateActiveVehicleCount" $ "No fleet owner found for linked vehicle of driver: " <> show driverId
  whenJust mbFleetOwnerId $ \fleetOwnerId -> incrementActiveVehicleCount fleetOwnerId
  where
    incrementActiveVehicleCount fleetOwnerId = do
      let totalActiveVehicleCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_VEHICLE_COUNT
      ensureRedisKeysExistForAllTimeCommon transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveVehicleCountKey Redis.incrby 1

decrementFleetOwnerAnalyticsActiveVehicleCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Maybe Text ->
  Id DP.Person ->
  m ()
decrementFleetOwnerAnalyticsActiveVehicleCount transporterConfig mbFleetOwnerId driverId = do
  when (isNothing mbFleetOwnerId) $ logTagError "AnalyticsUpdateActiveVehicleCount" $ "No fleet owner found for linked vehicle of driver: " <> show driverId
  whenJust mbFleetOwnerId $ \fleetOwnerId -> decrementActiveVehicleCount fleetOwnerId
  where
    decrementActiveVehicleCount fleetOwnerId = do
      let totalActiveVehicleCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_VEHICLE_COUNT
      ensureRedisKeysExistForAllTimeCommon transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveVehicleCountKey Redis.decrby 1

-- | Compute period dashboard analytics via ClickHouse for a given operator and time window
computePeriodOperatorAnalytics ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  Day ->
  Day ->
  m PeriodRes
computePeriodOperatorAnalytics transporterConfig operatorId fromDay toDay = do
  let useDBForAnalytics = transporterConfig.analyticsConfig.useDbForEarningAndMetrics
  driverIds <- SDFStatus.getFleetDriverIdsAndDriverIdsByOperatorId operatorId
  activeDriver <- SDFStatus.getTotalFleetDriverAndDriverCountByOperatorIdInDateRange operatorId (UTCTime fromDay 0) (UTCTime toDay 86399)
  enabledCount <- CDI.getEnabledDriverCountByDriverIds driverIds (UTCTime fromDay 0) (UTCTime toDay 86399)
  -- gt1 <- CDaily.countDriversWithNumRidesGreaterThan1Between driverIds fromDay toDay
  -- gt10 <- CDaily.countDriversWithNumRidesGreaterThan10Between driverIds fromDay toDay
  -- gt50 <- CDaily.countDriversWithNumRidesGreaterThan50Between driverIds fromDay toDay

  (approvedDriverInspection, approvedVehicleInspection, rejectedDriverInspection, rejectedVehicleInspection) <-
    if useDBForAnalytics
      then QFleetOpsDailyExtra.sumApprovedVehicleAndDriverRequestsByFleetOperatorIdsAndDateRangeDB operatorId fromDay toDay
      else CFleetOpDailyStats.sumApprovedDriverAndVehicleRequestsByFleetOperatorIdsAndDateRange operatorId fromDay toDay

  pure PeriodRes {activeDriver, driverEnabled = enabledCount, greaterThanOneRide = 0 :: Int, greaterThanTenRide = 0 :: Int, greaterThanFiftyRide = 0 :: Int, approvedDriverInspection, approvedVehicleInspection, rejectedDriverInspection, rejectedVehicleInspection}

calculateDayNumberOfWeek :: Day -> Integer
calculateDayNumberOfWeek = fromIntegral . (\d -> d - 1) . fromEnum . dayOfWeek

-- | Common function to handle driver analytics and flow status caching
handleDriverAnalyticsAndFlowStatus ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Id DP.Person ->
  Maybe DI.DriverInformation ->
  (DI.DriverInformation -> m ()) -> -- analytics action to perform
  (DI.DriverInformation -> m ()) -> -- flow status action to perform
  m ()
handleDriverAnalyticsAndFlowStatus transporterConfig driverId mbDriverInfo analyticsAction flowStatusAction = do
  let allowCacheDriverFlowStatus = transporterConfig.analyticsConfig.allowCacheDriverFlowStatus
  let needsDriverInfo = transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics || allowCacheDriverFlowStatus

  when needsDriverInfo $ do
    driverInfo <- case mbDriverInfo of
      Just driverInfo -> pure driverInfo
      Nothing -> QDI.findById driverId >>= fromMaybeM (DriverNotFound driverId.getId)

    when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
      analyticsAction driverInfo

    when allowCacheDriverFlowStatus $ do
      flowStatusAction driverInfo
