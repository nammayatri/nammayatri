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
import Data.Either (lefts)
import Data.List (nub, sort)
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
import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.AnalyticsExtra
import qualified SharedLogic.CancellationFault as CancellationFault
import qualified SharedLogic.DriverFlowStatus as SDFStatus
import SharedLogic.FleetAnalytics.Realtime
import qualified SharedLogic.FleetOperatorStats as SFleetOperatorStats
import qualified Data.Set as Set
import qualified Lib.Finance.Domain.Types.Account as Account
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.Account as QAccount
import qualified Storage.Clickhouse.DailyStats as CDaily
import qualified Storage.Clickhouse.DriverInformation as CDI
import qualified Storage.Clickhouse.DriverOperatorAssociation as CDOA
import qualified Storage.Clickhouse.FleetOperatorAssociation as CFOA
import qualified Storage.Clickhouse.FleetOperatorDailyStats as CFleetOpDailyStats
import qualified Storage.Clickhouse.VehicleRegistrationCertificate as CVRC
import qualified Storage.Queries.DailyStatsExtra as QDailyStatsExtra
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.DriverRCAssociationExtra as QDRCAssoc
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.DriverStatsExtra as QDriverStatsExtra
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOperatorDailyStatsExtra as QFleetOpsDailyExtra
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubscriptionPurchaseExtra
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as QVRCExtra
import Tools.Error hiding (CustomerCancelled)

-- | Use CH for online-duration KPIs only when ops has set a cutoff and the
-- window starts on/after that merchant-local day. null cutoff → always PG.
useClickhouseForOnlineDuration :: Maybe Day -> Day -> Bool
useClickhouseForOnlineDuration mbCutoff fromDay =
  maybe False (fromDay >=) mbCutoff

data OperatorAllTimeKpis = OperatorAllTimeKpis
  { averageLoginHours :: Maybe Double,
    greaterThanOneRide :: Maybe Int,
    greaterThanTenRide :: Maybe Int,
    greaterThanFiftyRide :: Maybe Int,
    incompleteFirstSubscription :: Maybe Int,
    documentSubmissionCompletedDrivers :: Maybe Int,
    documentSubmissionCompletedVehicles :: Maybe Int,
    planActiveDrivers :: Maybe Int,
    lowSubscriptionBalanceDrivers :: Maybe Int,
    inactiveDrivers30 :: Maybe Int,
    inactiveDrivers60 :: Maybe Int,
    inactiveDrivers90 :: Maybe Int
  }

-- | Extra overall / live KPIs for operator all-time analytics (no new endpoint).
computeOperatorAllTimeKpis ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    BeamFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  m OperatorAllTimeKpis
computeOperatorAllTimeKpis transporterConfig operatorId = do
  localNow <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let today = utctDay localNow
      lowBalanceThreshold = transporterConfig.analyticsConfig.lowSubscriptionBalanceThreshold
      inactiveHoursThreshold = transporterConfig.analyticsConfig.inactiveAvgLoginHoursPerDayThreshold
      onlineDurationChFrom = transporterConfig.analyticsConfig.onlineDurationClickhouseFrom
      -- Lifetime rides: fixed epoch (not onlineDurationCalculateFrom)
      lifetimeFrom = fromGregorian 2018 1 1
      -- "2nd BOT" / enabled semantics for login hrs only when BOT flow is on
      botFlow = transporterConfig.enableBotFlow == Just True

  driverIdsRaw <- SDFStatus.getFleetDriverIdsAndDriverIdsByOperatorId operatorId
  let driverIds = nub driverIdsRaw
      ownerIds = map getId driverIds

  -- Lifetime ride buckets (>=1 / >=10 / >=50 ≈ sum rides > 0 / 9 / 49)
  (gt1, gt10, gt50) <-
    if null driverIds
      then pure (0, 0, 0)
      else do
        a <- CDaily.countDriversWithNumRidesGreaterThanBetween driverIds lifetimeFrom today 0
        b <- CDaily.countDriversWithNumRidesGreaterThanBetween driverIds lifetimeFrom today 9
        c <- CDaily.countDriversWithNumRidesGreaterThanBetween driverIds lifetimeFrom today 49
        pure (a, b, c)

  -- Avg login hrs = Σ online hrs of enabled drivers ÷ # enabled.
  -- Clar ties this to BOT check → only compute when enableBotFlow is on.
  -- Lifetime window starts at epoch → cutoff routes this to PG (driver_stats) unless
  -- onlineDurationClickhouseFrom is set at/before lifetimeFrom.
  averageLoginHours <-
    if not botFlow || null driverIds
      then pure Nothing
      else do
        enabledIds <- CDI.getEnabledDriverIds driverIds
        totalOnlineSec <-
          if null enabledIds
            then pure 0
            else
              if useClickhouseForOnlineDuration onlineDurationChFrom lifetimeFrom
                then CDaily.sumOnlineDurationSeconds enabledIds
                else do
                  stats <- QDriverStatsExtra.findAllByDriverIdList (cast <$> enabledIds)
                  pure $ sum $ map ((.getSeconds) . (.onlineDuration)) stats
        let enabledCount = length enabledIds
        pure $
          if enabledCount > 0
            then Just (fromIntegral totalOnlineSec / 3600.0 / fromIntegral enabledCount)
            else Just 0

  -- Plan-active now (all associated)
  planActiveOwnerIds <- QSubscriptionPurchaseExtra.findActiveOwnerIdsByOwnerIds ownerIds DSP.DRIVER
  let planActiveSet = Set.fromList planActiveOwnerIds
      planActiveDrivers = Set.size planActiveSet

  -- Incomplete first subscription: DCO (operator-code) associations − completed/active first purchase.
  -- Fleet-mediated drivers excluded until product confirms roster scope.
  dcoDriverIds <- CDOA.getDriverIdsByOperatorId operatorId
  let dcoOwnerIds = map getId dcoDriverIds
  purchasedCount <-
    QSubscriptionPurchaseExtra.findDistinctOwnersWithStatuses
      dcoOwnerIds
      DSP.DRIVER
      [DSP.ACTIVE, DSP.EXPIRED, DSP.EXHAUSTED]
  let incompleteFirstSubscription = max 0 (length dcoDriverIds - purchasedCount)

  -- Document submission completed (proxy: verified = all required docs approved)
  statusCounts <- CDI.getStatusCountsByDriverIds driverIds
  let documentSubmissionCompletedDrivers =
        sum [n | (Just True, _, n) <- statusCounts]

  -- Vehicles: operator→driver→RC ∪ operator→fleet→RC (distinct verified)
  assocs <- QDRCAssoc.findAllActiveByDriverIds driverIds
  let driverRcIds = Set.fromList $ map (.rcId) assocs
  rcs <- QVRCExtra.findAllById (Set.toList driverRcIds)
  let verifiedDriverRcIds = Set.fromList $ map (.id) $ filter (\rc -> rc.verified == Just True) rcs
  fleetOwnerIds <- CFOA.getFleetOwnerIdsByOperatorId operatorId
  verifiedFleetRcIds <- Set.fromList <$> CVRC.findVerifiedIdsByFleetOwnerIds fleetOwnerIds
  let documentSubmissionCompletedVehicles =
        Set.size $ verifiedDriverRcIds <> verifiedFleetRcIds

  -- Low subscription balance: RideCredit among drivers with an active plan only
  -- (no plan / no account ⇒ excluded)
  accounts <-
    if null planActiveOwnerIds
      then pure []
      else
        QAccount.findAllByCounterpartyIdsAndType
          (map Just planActiveOwnerIds)
          (Just Account.DRIVER)
          Account.RideCredit
          Nothing
  let lowBalanceTotal =
        Set.size $
          Set.fromList $
            mapMaybe
              ( \acc ->
                  acc.counterpartyId >>= \cid ->
                    if Set.member cid planActiveSet && acc.balance < lowBalanceThreshold
                      then Just cid
                      else Nothing
              )
              accounts

  let countInactive days = do
        let fromDay = addDays (negate (fromIntegral days - 1)) today
        perDriver <-
          if useClickhouseForOnlineDuration onlineDurationChFrom fromDay
            then CDaily.sumOnlineDurationSecondsByDriverBetween driverIds fromDay today
            else QDailyStatsExtra.sumOnlineDurationSecondsByDriverBetween driverIds fromDay today
        let avgHrs secs = fromIntegral secs / 3600.0 / fromIntegral days
            -- No daily_stats row in the window ⇒ inactive
            withRows = Set.fromList $ map (getId . fst) perDriver
            inactiveWithRows = length $ filter (\(_, secs) -> avgHrs secs < inactiveHoursThreshold) perDriver
            missing = length $ filter (\d -> getId d `Set.notMember` withRows) driverIds
        pure $ inactiveWithRows + missing

  inactiveDrivers30 <- countInactive (30 :: Int)
  inactiveDrivers60 <- countInactive (60 :: Int)
  inactiveDrivers90 <- countInactive (90 :: Int)

  pure
    OperatorAllTimeKpis
      { averageLoginHours,
        greaterThanOneRide = Just gt1,
        greaterThanTenRide = Just gt10,
        greaterThanFiftyRide = Just gt50,
        incompleteFirstSubscription = Just incompleteFirstSubscription,
        documentSubmissionCompletedDrivers = Just documentSubmissionCompletedDrivers,
        documentSubmissionCompletedVehicles = Just documentSubmissionCompletedVehicles,
        planActiveDrivers = Just planActiveDrivers,
        lowSubscriptionBalanceDrivers = Just lowBalanceTotal,
        inactiveDrivers30 = Just inactiveDrivers30,
        inactiveDrivers60 = Just inactiveDrivers60,
        inactiveDrivers90 = Just inactiveDrivers90
      }

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
  -- | the fault verdict's party, Nothing in cities with no fault rules configured
  Maybe CancellationFault.FaultParty ->
  m ()
updateCancellationAnalyticsAndDriverStats transporterConfig ride source mbAtFault = do
  driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  -- Fleet-operator analytics describe the event, so they stay keyed on who pressed cancel.
  case source of
    SBCR.ByDriver -> recordFleetOperatorAnalyticsImpl transporterConfig (DriverCancelled ride.driverId ride.id.getId)
    SBCR.ByUser -> recordFleetOperatorAnalyticsImpl transporterConfig (CustomerCancelled ride.driverId ride.id.getId)
    _ -> pure ()
  -- The "valid ... cancellation" counters are an attribution, not a record of who pressed the
  -- button, so they follow the fault verdict: a driver who stalls until the rider cancels used
  -- to land in the CUSTOMER bucket. Cities with no fault rules keep the old source-based split.
  case attributedParty of
    Just CancellationFault.DriverAtFault ->
      QDriverStats.updateValidDriverCancellationTagCount (driverStats.validDriverCancellationTagCount + 1) ride.driverId
    Just CancellationFault.CustomerAtFault ->
      QDriverStats.updateValidCustomerCancellationTagCount (driverStats.validCustomerCancellationTagCount + 1) ride.driverId
    -- SharedFault / NoFault are attributed to nobody
    _ -> pure ()
  where
    attributedParty = case mbAtFault of
      Just party -> Just party
      Nothing -> case source of
        SBCR.ByDriver -> Just CancellationFault.DriverAtFault
        SBCR.ByUser -> Just CancellationFault.CustomerAtFault
        _ -> Nothing

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
-- to one path and forgotten on the other. The request publishes this fact when
-- fleet operator dashboard analytics is enabled. The fleet-analytics consumer applies it.
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
  | RatingSubmitted (Id DP.Person) Int Bool Text Int Text
  deriving (Generic, Show, ToJSON, FromJSON)

class Monad m => PublishesFleetAnalytics m where
  recordFleetOperatorAnalytics :: TC.TransporterConfig -> FleetOperatorAnalytics -> m ()

-- Any request monad with a Kafka producer, so the driver app, the allocator and the
-- ride-events consumer share one instance. Applying the fact stays in the fleet-analytics consumer.
instance
  ( MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  PublishesFleetAnalytics m
  where
  recordFleetOperatorAnalytics = recordFleetOperatorAnalyticsImpl

recordFleetOperatorAnalyticsImpl ::
  ( MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  TC.TransporterConfig ->
  FleetOperatorAnalytics ->
  m ()
recordFleetOperatorAnalyticsImpl transporterConfig fact =
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $
    void $ publishFleetRealtimeEvent transporterConfig (fleetEventId fact) fact

-- | Postgres increments, and for the operator the Redis counters in the same lock hold. The
-- fleet-analytics consumer passes a per-entity claim as claimEntity. claimEntity runs the Postgres
-- writes, then the Redis counters, inside the entity lock.
applyFleetOperatorAnalytics ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Forkable m,
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
      fleetAssociations <- QFDA.findAllByDriverIds driverIds
      let fleetDriverPairs = [(fa.fleetOwnerId, fa.driverId.getId) | fa <- fleetAssociations]
      operatorAwaitable <-
        if null operatorDriverPairs
          then pure Nothing
          else fmap Just $
            awaitableFork "AnalyticsSearchOperator" $
              SFleetOperatorStats.incrementTotalRequestCountBatch operatorDriverPairs transporterConfig claimEntity $ \operatorId driverCount ->
                incrOperatorCounter transporterConfig operatorId TOTAL_REQUEST_COUNT (fromIntegral driverCount)
      fleetAwaitable <-
        if null fleetDriverPairs
          then pure Nothing
          else
            fmap Just $
              awaitableFork "AnalyticsSearchFleet" $
                SFleetOperatorStats.incrementTotalRequestCountBatch fleetDriverPairs transporterConfig claimEntity (\_ _ -> pure ())
      awaitFleetAnalytics (catMaybes [operatorAwaitable, fleetAwaitable])
    RatingSubmitted driverId ratingValue shouldIncrementCount _rideId _totalRatingCount _submissionId ->
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
      mbFleetOwner <- QFDA.findByDriverId driverId True
      when (isNothing mbFleetOwner) $ logTagInfo tag $ "No fleet owner found for driver: " <> show driverId
      operatorAwaitables <-
        forM operatorIds $ \operatorId ->
          awaitableFork tag $
            Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId) 10 5000 $
              claimEntity
                operatorId
                (step operatorId)
                (forM_ operatorCounters $ \(metric, amount) -> incrOperatorCounter transporterConfig operatorId metric amount)
      fleetAwaitable <-
        forM mbFleetOwner $ \fleetOwner ->
          awaitableFork tag $
            Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId) 10 5000 $
              claimEntity fleetOwner.fleetOwnerId (step fleetOwner.fleetOwnerId) (pure ())
      awaitFleetAnalytics (operatorAwaitables <> maybeToList fleetAwaitable)
    awaitFleetAnalytics awaitables = do
      results <- mapM (L.await Nothing) awaitables
      case lefts results of
        [] -> pure ()
        (err : _) -> throwError $ InternalError $ "fleet analytics apply failed: " <> show err

applyPublishedFleetAnalytics ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Forkable m,
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
  RatingSubmitted _ _ _ rideId _ submissionId ->
    "rating.submitted:" <> rideId <> ":" <> submissionId

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
      ensureRedisKeysExistForAllTimeCommon False transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveDriverCountKey Redis.incrby 1

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
      ensureRedisKeysExistForAllTimeCommon False transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveDriverCountKey Redis.decrby 1

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
      ensureRedisKeysExistForAllTimeCommon False transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveVehicleCountKey Redis.incrby 1

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
      ensureRedisKeysExistForAllTimeCommon False transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveVehicleCountKey Redis.decrby 1

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
