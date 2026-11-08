-- | SESSIONIZER: turns LTS pings (location-updates) and driver analytics
-- events (driver-analytics-events) into 5-minute per-driver rows in
-- ClickHouse. A row is (bucket, city, driver, driver state): the driver was in
-- that state during that 5-minute bucket. Every ping whose mode is not OFFLINE
-- adds to the row of the bucket of its st and of the state in force at its st,
-- marking it online, except a point the phone recorded on a ride and sent later
-- ('placePing'), which goes at its ts; every event adds its counts to the row of its own time
-- and the state in force then; a state change inside a bucket gives another
-- row. Online time is not computed here: it is the number of distinct online
-- buckets per driver, times 5 minutes. Both topics are keyed by driver id with
-- the same partition count, so one pod holds all of a driver's records.
--
-- Rows go to atlas_kafka.driver_analytics_5m through the 'analyticsTopic'
-- topic and a ClickHouse Kafka engine table. The table is a SummingMergeTree
-- keyed by state_id ('stateIdOf' the state), so rows for the same key from
-- different flushes or pods add up.
--
-- Driver state. The sessionizer reads every partition of driver-pool-state
-- itself, on a second consumer that commits nothing, so every pod sees every
-- state event whatever the partitioning of location-updates. It applies each
-- event the way the DRIVER_POOL_STATE writer applies it to Redis and keeps
-- each driver's state in memory for good; history is trimmed at each flush to
-- what held records can still need. Redis is read only for a driver not in
-- memory (after a restart, or the first time a driver is seen), once the
-- writer is past both the needed time and this consumer's start; its events
-- already in that copy are skipped. A driver the writer has not seeded from
-- the DB yet is dropped from memory on each of its state events and loaded
-- again with its next record, since only the writer can seed it. A record
-- waits, without a timeout, until every driver-pool-state event published up
-- to 'stateBufferMs' after its time has been read (a high-watermark probe of
-- the state partitions) and, for a driver not in memory, until the writer is
-- that far too. Waiting never blocks: both consumers keep polling, and held
-- records stay uncommitted. Lag on the publishing side is assumed not to
-- happen. An event or buffered ping older than the history kept in memory (late,
-- or replayed) takes its state from the writer's Redis history instead.
--
-- Every 'flushEveryMs' the rows are produced to 'analyticsTopic' and every
-- delivery is awaited. Then the last applied offset per partition
-- (sessionizer:flushed:*) is written to Redis, those offsets are committed
-- (held records stay uncommitted) and sessionizer:watermark is published.
-- Records at or below a partition's flushed offset are skipped, so a crash
-- before the commit does not count them again. A crash between the
-- deliveries and the Redis write does.
module Processor.Sessionizer.Processor
  ( runSessionizer,
  )
where

import Control.Applicative ((<|>))
import qualified Control.Concurrent as CC
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Either as Either
import qualified Data.HashMap.Strict as HM
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import qualified Data.Scientific as Sci
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import qualified Database.Redis as R
import Environment
import qualified EulerHS.Runtime as L
import qualified Kafka.Consumer as Consumer
import qualified Kafka.Metadata as KM
import qualified Kafka.Producer as Producer
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Flow
import Kernel.Utils.Common hiding (id)
import Processor.ClickhouseText (epochMsText)
import Processor.DriverPoolState.Types
import qualified Processor.LocationUpdate.Types as LU
import Processor.StateColumns (renderStateValues, stampVehicleChange, stateColumnKeys, stateIdOf, stateOfTag, stateTag, statesAt)
import qualified Prometheus as P
import Transporter.Kafka.Committing (isRdKafkaError)
import qualified Prelude as P

locationTopic, eventsTopic, stateTopic, analyticsTopic :: Text
locationTopic = "location-updates"
eventsTopic = "driver-analytics-events"
stateTopic = "driver-pool-state"
analyticsTopic = "driver-analytics-5m"

bucketMs, flushEveryMs, flushRetryMs, stateProbeEveryMs, writerReadEveryMs :: Integer
bucketMs = 5 * 60 * 1000
flushEveryMs = 5 * 60 * 1000
flushRetryMs = 30 * 1000
stateProbeEveryMs = 1000
writerReadEveryMs = 1000

-- | Event count columns of driver_analytics_5m; money columns are Decimal(18,
-- 4), held here in ten-thousandths. Any other key in an event is ignored.
eventColumns, moneyColumns :: [Text]
eventColumns =
  [ "popups_sent",
    "popups_accepted",
    "popups_rejected",
    "popups_pulled",
    "response_secs_sum",
    "auto_accept_search_requests",
    "total_bookings",
    "rides_started",
    "finished_rides",
    "cancelled_rides",
    "rides_cancelled_by_driver",
    "rides_cancelled_by_user",
    "auto_accept_bookings",
    "auto_accept_finished_rides",
    "auto_accept_cancelled_rides",
    "auto_accept_rides_cancelled_by_driver",
    "auto_accept_rides_cancelled_by_user",
    "rides_with_rating",
    "rating_sum"
  ]
    <> moneyColumns
moneyColumns = ["driver_addition", "customer_tip", "gross_earnings"]

-- | Pings stamped more than this far ahead of the wall clock, or before
-- 'minPlausiblePingMs' (2020-01-01), are skipped: one future ping would
-- otherwise push the sessionizer watermark into the future.
maxFutureSkewMs, minPlausiblePingMs :: Integer
maxFutureSkewMs = 5 * 60 * 1000
minPlausiblePingMs = 1577836800000

-- | A ping whose st is more than this after its ts was recorded by the phone and
-- sent later, when it had network again. LTS gives a whole batch one st.
bufferedPingGapMs :: Integer
bufferedPingGapMs = 60 * 1000

pollBatchSize, loadChunk, rowsPerMessage :: Int
pollBatchSize = 1000
loadChunk = 500

-- | JSONEachRow rows per Kafka message (~1.5 KB a row).
rowsPerMessage = 200

ignoredDriverIds :: [Text]
ignoredDriverIds = ["", "drivers-not-found"]

-- | A ping that marks its row online.
data PingAt = PingAt
  { pingDriverId :: Text,
    pingStMs :: Integer,
    pingTsMs :: Integer,
    pingCity :: Text,
    pingLat :: Double,
    pingLon :: Double
  }

-- | One driver-analytics-events record (SharedLogic.DriverPool.DriverAnalyticsEvent).
data AnalyticsEvent = AnalyticsEvent
  { driverId :: Text,
    -- | epoch ms
    ts :: Integer,
    merchantOperatingCityId :: Text,
    counts :: A.Object
  }
  deriving (Generic, FromJSON)

data Decoded
  = -- | driver id, the update, its st in epoch ms
    DecodedPing Text LU.LocationUpdates Integer
  | DecodedEvent AnalyticsEvent

-- | A location or event record waiting until the state at its time has been read.
data HeldRecord = HeldRecord
  { hrHeldAtMs :: Integer,
    hrRecord :: ConsumerRecordD,
    -- | Nothing: not countable
    hrDecoded :: Maybe Decoded
  }

-- | Measures for one row.
data Activity = Activity
  { pings :: Integer,
    online :: Bool,
    -- | (st in epoch ms, lat, lon) of the row's newest ping
    lastPing :: Maybe (Integer, Double, Double),
    -- | state_at: the ts of the history entry the row's state comes from
    stateAtMs :: Integer,
    -- | event column -> sum (money in ten-thousandths)
    counts :: M.Map Text Integer
  }

instance Semigroup Activity where
  a <> b =
    Activity
      { pings = a.pings + b.pings,
        online = a.online || b.online,
        lastPing = max a.lastPing b.lastPing,
        stateAtMs = max a.stateAtMs b.stateAtMs,
        counts = M.unionWith (+) a.counts b.counts
      }

instance Monoid Activity where
  mempty = Activity 0 False Nothing 0 M.empty

-- | (bucket start ms, city, driver, the driver's state as a 'stateTag';
-- Nothing when it is unknown)
type ActivityKey = (Integer, Text, Text, Maybe Text)

-- | One driver's state as this consumer applied it.
data DriverMem = DriverMem
  { -- | the state-column fields of the writer's current-state hash, merged event by event
    dmCurrent :: A.Object,
    -- | history entries: event ts -> snapshot
    dmHistory :: M.Map Integer DriverStateSnapshot,
    -- | events at or before this time were already in the Redis copy it was loaded from
    dmLoadedThroughMs :: Integer,
    dmSeeded :: Bool
  }

-- | Once every driver-pool-state partition is consumed up to its recorded
-- high watermark, every event published before probeAtMs has been applied.
data Probe = Probe
  { probeAtMs :: Integer,
    probeEnds :: M.Map Int Int64
  }

-- | A driver not in memory (or not seeded by the writer when last loaded)
-- with state events read: loaded once the writer is past them.
data PendingLoad = PendingLoad
  { -- | the newest event ts
    plNeededMs :: Integer,
    -- | when the first of them was read
    plQueuedAtMs :: Integer,
    -- | newest first
    plEvents :: [DriverPoolStateEvent]
  }

-- | For alerting, served on the metrics port.
data Metrics = Metrics
  { oldestHeldAgeMs :: P.Gauge,
    -- | now minus the time every driver-pool-state event before it has been read
    stateReadLagMs :: P.Gauge,
    -- | now minus the state writer's watermark
    writerLagMs :: P.Gauge,
    -- | events and buffered pings whose state came from the writer's Redis history, not memory
    historyFallbacks :: P.Counter,
    -- | flushes that failed to produce to the analytics topic
    flushFailures :: P.Counter
  }

registerMetrics :: IO Metrics
registerMetrics =
  Metrics
    <$> regGauge "sessionizer_oldest_held_age_ms" "How long the oldest held record has waited"
    <*> regGauge "sessionizer_state_read_lag_ms" "Now minus the time up to which every driver-pool-state event has been read"
    <*> regGauge "sessionizer_writer_lag_ms" "Now minus the state writer's watermark"
    <*> P.register (P.counter (P.Info "sessionizer_history_fallbacks" "Events and buffered pings whose state came from the writer's Redis history"))
    <*> P.register (P.counter (P.Info "sessionizer_flush_failures" "Flushes that failed to produce to the analytics topic"))
  where
    regGauge name help = P.register (P.gauge (P.Info name help))

data Acc = Acc
  { metrics :: Metrics,
    activity :: M.Map ActivityKey Activity,
    -- | Last record time handled per (topic, partition) since the last flush.
    recordProgress :: M.Map (Text, Int) Integer,
    -- | First and last applied offset per (topic, partition) since the last flush.
    offsets :: M.Map (Text, Int) (Int64, Int64),
    nextFlushMs :: Integer,
    drivers :: M.Map Text DriverMem,
    -- | every driver-pool-state event published before this time has been applied
    stateReadyMs :: Integer,
    probe :: Maybe Probe,
    probedAtMs :: Integer,
    -- | oldest first
    heldRecords :: [HeldRecord],
    -- | drivers to load from Redis once the writer has applied their events
    pendingLoad :: M.Map Text PendingLoad,
    -- | the writer's watermark (epoch ms) as last read, and when it was read
    writerWmMs :: Integer,
    writerReadAtMs :: Integer,
    -- | next driver-pool-state offset to read, per partition
    stateNext :: M.Map Int Int64,
    -- | last offset already flushed per (topic, partition), read from Redis
    -- the first time a partition is seen in a window (-1: none)
    flushedThrough :: M.Map (Text, Int) Int64
  }

runSessionizer :: L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
runSessionizer flowRt appEnv kc = do
  stateEnv <- maybe (P.error "driverStateRedisCfg is not set; SESSIONIZER needs it") pure appEnv.driverStateHedisEnv
  start <- nowMs
  (sc, stateStart, stateStartMs) <- openStateConsumer
  metrics <- registerMetrics
  pollLoop
    stateEnv
    sc
    stateStartMs
    Acc
      { metrics,
        activity = M.empty,
        recordProgress = M.empty,
        offsets = M.empty,
        nextFlushMs = start + flushEveryMs,
        drivers = M.empty,
        stateReadyMs = 0,
        probe = Nothing,
        probedAtMs = 0,
        heldRecords = [],
        pendingLoad = M.empty,
        writerWmMs = 0,
        writerReadAtMs = 0,
        stateNext = stateStart,
        flushedThrough = M.empty
      }
  where
    flow :: Flow a -> IO a
    flow = runFlowR flowRt appEnv

    -- Every partition of driver-pool-state, from its current end, on a
    -- consumer of its own. hw-kafka-client cannot open a consumer without a
    -- subscription, so it subscribes under a group id unique to this process:
    -- as the group's only member it is given every partition, and nothing is
    -- committed. Once assigned, every partition is moved to its end.
    -- Partitions added to the topic later are not picked up until a restart.
    openStateConsumer = do
      openedAt <- nowMs
      let props =
            appEnv.kafkaConsumerCfg.consumerProperties
              <> Consumer.groupId (Consumer.ConsumerGroupId ("sessionizer-state-" <> show openedAt))
              <> Consumer.noAutoCommit
      sc <- either (P.error . ("sessionizer: cannot open the driver-pool-state consumer: " <>) . show) P.id <$> Consumer.newConsumer props (Consumer.topics [Consumer.TopicName stateTopic])
      partitionCount <- length <$> retryingIO "driver-pool-state partition list" (sequence <$> KM.watermarkOffsets sc (Consumer.Timeout 10000) (Consumer.TopicName stateTopic))
      awaitAssignment sc partitionCount
      wos <- retryingIO "driver-pool-state high watermarks" (sequence <$> KM.watermarkOffsets sc (Consumer.Timeout 10000) (Consumer.TopicName stateTopic))
      -- Every event published before now is below these offsets, so in the
      -- writer's copy once its watermark passes now.
      startedMs <- nowMs
      let starts = M.fromList [(Consumer.unPartitionId (KM.woPartitionId wo), Consumer.unOffset (KM.woHighWatermark wo)) | wo <- wos]
      seekErr <- Consumer.seek sc (Consumer.Timeout 10000) [Consumer.TopicPartition (Consumer.TopicName stateTopic) (Consumer.PartitionId p) (Consumer.PartitionOffset o) | (p, o) <- M.toList starts]
      whenJust seekErr $ \err -> P.error ("sessionizer: cannot move the driver-pool-state partitions to their end: " <> show err)
      flow $ logInfo $ "sessionizer: reading all " <> show (M.size starts) <> " driver-pool-state partitions from " <> show (M.toList starts)
      pure (sc, starts, startedMs)

    -- Polls (discarding: every partition is moved to its end next) until all
    -- partitions are assigned.
    awaitAssignment sc partitionCount = do
      _ <- Consumer.pollMessageBatch sc (Consumer.Timeout 500) (Consumer.BatchSize pollBatchSize)
      assigned <- Consumer.assignment sc
      let got = either (const 0) (length . fromMaybe [] . M.lookup (Consumer.TopicName stateTopic)) assigned
      unless (got >= partitionCount) $ do
        flow $ logInfo $ "sessionizer: waiting for the driver-pool-state assignment (" <> show got <> "/" <> show partitionCount <> ")"
        awaitAssignment sc partitionCount

    retryingIO what action = do
      res <- action `catchAny` (pure . Left . Consumer.KafkaError . show)
      case res of
        Right v | not (null v) -> pure v
        other -> do
          flow $ logError $ "sessionizer: " <> what <> " unreadable, retrying: " <> either show (const "no partitions") other
          CC.threadDelay 1000000
          retryingIO what action

    pollLoop stateEnv sc stateStartMs acc = do
      statePolled <- Consumer.pollMessageBatch sc (Consumer.Timeout 50) (Consumer.BatchSize pollBatchSize)
      polled <- Consumer.pollMessageBatch kc (Consumer.Timeout 450) (Consumer.BatchSize pollBatchSize)
      forM_ (Either.lefts statePolled <> Either.lefts polled) $ \err ->
        unless (isRdKafkaError "RdKafkaRespErrTimedOut" err) $ flow $ logError $ "sessionizer: kafka poll error: " <> show err
      let stateRecords = filter (isTopic stateTopic) (Either.rights statePolled)
          activityRecords = filter (\cr -> isTopic locationTopic cr || isTopic eventsTopic cr) (Either.rights polled)
      withFlushed <- loadFlushedRetrying stateEnv.hedisConnection acc (map topicPartition activityRecords)
      -- Already in ClickHouse: committed with the next flush, not counted again.
      let (skipped, fresh) = DL.partition (\cr -> Consumer.unOffset (Consumer.crOffset cr) <= M.findWithDefault (-1) (topicPartition cr) withFlushed.flushedThrough) activityRecords
      unless (null skipped) $ flow $ logInfo $ "sessionizer: skipped " <> show (length skipped) <> " records flushed before a restart"
      batchNow <- nowMs
      withWriter <- readWriter stateEnv batchNow (applyStateRecords batchNow (foldl' trackOffset withFlushed skipped) stateRecords)
      withLoaded <- loadPending stateEnv stateStartMs withWriter
      probed <- advanceProbe sc batchNow withLoaded {heldRecords = withLoaded.heldRecords <> map (holdRecord batchNow) fresh}
      released <- releaseRecords stateEnv stateStartMs batchNow probed
      now <- nowMs
      acc' <- if now >= released.nextFlushMs then flush stateEnv (null polled && null released.heldRecords) now released else pure released
      reportMetrics now acc'
      pollLoop stateEnv sc stateStartMs acc'

    reportMetrics now acc = do
      P.setGauge acc.metrics.oldestHeldAgeMs (maybe 0 (\hr -> fromIntegral (now - hr.hrHeldAtMs)) (listToMaybe acc.heldRecords))
      P.setGauge acc.metrics.stateReadLagMs (fromIntegral (now - acc.stateReadyMs))
      P.setGauge acc.metrics.writerLagMs (fromIntegral (now - acc.writerWmMs))

    -- The writer's watermark, read at most every 'writerReadEveryMs'.
    readWriter stateEnv now acc
      | now - acc.writerReadAtMs < writerReadEveryMs = pure acc
      | otherwise = do
        res <- readWriterWatermark stateEnv.hedisConnection
        case res of
          Left err -> flow (logWarning $ "sessionizer: state writer watermark unreadable: " <> err) >> pure acc {writerReadAtMs = now}
          Right mbWm -> pure acc {writerWmMs = max acc.writerWmMs (fromMaybe 0 mbWm), writerReadAtMs = now}

    -- Loads the drivers waiting for the writer whose events the writer has
    -- applied (and this consumer's start), then applies the events queued for
    -- them: those already in the loaded copy are skipped.
    loadPending stateEnv stateStartMs acc = do
      let (due, notYet) = M.partition (\pl -> acc.writerWmMs >= writerTarget stateStartMs pl.plNeededMs pl.plQueuedAtMs) acc.pendingLoad
      if M.null due
        then pure acc
        else do
          loaded <- hydrate stateEnv acc {pendingLoad = notYet} (M.keys due)
          pure (foldl' applyEvent loaded (concatMap (reverse . (.plEvents)) (M.elems due)))

    -- Loads drivers from the writer's Redis. Only called once the writer is
    -- past every event of theirs this consumer has read or skipped, so every
    -- event up to the writer's watermark is in that copy, and those after it
    -- are read here.
    hydrate stateEnv acc wanted = case S.toList (S.fromList [did | did <- wanted, did `notElem` ignoredDriverIds]) of
      [] -> pure acc
      dids -> do
        loaded <- loadDriversRetrying stateEnv.hedisConnection dids
        let mem (current, seeded, history) = DriverMem {dmCurrent = current, dmHistory = history, dmLoadedThroughMs = acc.writerWmMs, dmSeeded = seeded}
        pure acc {drivers = M.union (M.map mem loaded) acc.drivers}

    loadDriversRetrying conn dids = do
      res <- loadDrivers conn dids `catchAny` (pure . Left . show)
      case res of
        Right found -> pure found
        Left err -> do
          flow $ logError $ "sessionizer: driver state load failed, retrying: " <> err
          CC.threadDelay 1000000
          loadDriversRetrying conn dids

    loadFlushedRetrying conn acc tps = case S.toList (S.fromList [tp | tp <- tps, M.notMember tp acc.flushedThrough]) of
      [] -> pure acc
      missing -> do
        res <- loadFlushed conn missing `catchAny` (pure . Left . show)
        case res of
          Right found -> pure acc {flushedThrough = M.union acc.flushedThrough found}
          Left err -> do
            flow $ logError $ "sessionizer: flushed offset load failed, retrying: " <> err
            CC.threadDelay 1000000
            loadFlushedRetrying conn acc tps

    -- Retried until it lands: until then a restart would count the window again.
    writeFlushedRetrying conn offs = do
      res <- writeFlushed conn offs `catchAny` (pure . Left . show)
      case res of
        Right () -> pure ()
        Left err -> do
          flow $ logError $ "sessionizer: flushed offset write failed, retrying: " <> err
          CC.threadDelay 1000000
          writeFlushedRetrying conn offs

    -- A finished probe moves stateReadyMs to its time. A new one is taken
    -- when the oldest held ping needs a later state, at most every
    -- 'stateProbeEveryMs'. A probe is kept until it is reached: no timeout.
    advanceProbe sc now acc = do
      let checked = case acc.probe of
            Just pr | probeReached acc pr -> acc {stateReadyMs = max acc.stateReadyMs pr.probeAtMs, probe = Nothing}
            _ -> acc
          waiting = maybe False (\hr -> neededMs hr > checked.stateReadyMs) (listToMaybe checked.heldRecords)
      if isNothing checked.probe && waiting && now - checked.probedAtMs >= stateProbeEveryMs
        then do
          newProbe <- takeProbe sc checked now
          pure checked {probe = newProbe, probedAtMs = now}
        else pure checked

    -- The high watermark of each driver-pool-state partition not read to it yet.
    takeProbe sc acc now = do
      marks <- sequence <$> KM.watermarkOffsets sc (Consumer.Timeout 5000) (Consumer.TopicName stateTopic)
      case marks of
        Left err -> do
          flow $ logWarning $ "sessionizer: state partition watermarks unreadable, probe deferred: " <> show err
          pure Nothing
        Right wos ->
          pure $
            Just
              Probe
                { probeAtMs = now,
                  probeEnds =
                    M.fromList
                      [ (p, high)
                        | wo <- wos,
                          let p = Consumer.unPartitionId (KM.woPartitionId wo)
                              high = Consumer.unOffset (KM.woHighWatermark wo),
                          M.findWithDefault 0 p acc.stateNext < high
                      ]
                }

    -- Held records go in order once the state topic has been read
    -- 'stateBufferMs' past their time and, for a driver not in memory, the
    -- writer too. No timeout: a record is never tagged with a state not yet read.
    releaseRecords stateEnv stateStartMs now acc = do
      let ready hr =
            neededMs hr <= acc.stateReadyMs
              && maybe True (\(did, atMs) -> M.notMember did acc.pendingLoad && (M.member did acc.drivers || acc.writerWmMs >= writerTarget stateStartMs atMs hr.hrHeldAtMs)) (heldDriverAt hr)
          (released, waiting) = span ready acc.heldRecords
      if null released then pure acc else processRecords stateEnv now acc {heldRecords = waiting} released

    -- Each countable ping marks the row of its bucket and of the state in
    -- force at its st online; each event adds its counts to the row of its
    -- bucket and of the state in force at its time.
    processRecords stateEnv batchNow acc released = do
      let plausible t = t >= minPlausiblePingMs && t <= batchNow + maxFutureSkewMs
          decoded = [(hr, d) | hr <- released, Just d <- [hr.hrDecoded], plausible (snd (decodedAt d))]
          pingsAt =
            [ PingAt {pingDriverId = did, pingStMs = st, pingTsMs = utcToMs lu.ts, pingCity = fromMaybe "" lu.mocid, pingLat = lu.pt.lat, pingLon = lu.pt.lon}
              | (_, DecodedPing did lu st) <- decoded,
                lu.mode /= Just "OFFLINE"
            ]
          eventsAt = [ev | (_, DecodedEvent ev) <- decoded]
          progressed =
            (foldl' trackOffset acc (map (.hrRecord) released))
              { recordProgress = foldl' (\m (tp, t) -> M.insert tp t m) acc.recordProgress [(topicPartition hr.hrRecord, snd (decodedAt d)) | (hr, d) <- decoded]
              }
      loaded <- hydrate stateEnv progressed [did | (_, d) <- decoded, let did = fst (decodedAt d), M.notMember did progressed.drivers]
      -- Older than the history kept in memory: the writer's history.
      let late =
            filter
              (\(did, atMs) -> isNothing (entryAt loaded did atMs))
              ([(p.pingDriverId, p.pingTsMs) | p <- pingsAt, isBufferedPing p] <> [(ev.driverId, ev.ts) | ev <- eventsAt])
      fallback <- if null late then pure M.empty else lateStatesRetrying stateEnv.hedisConnection late
      void $ P.addCounter acc.metrics.historyFallbacks (fromIntegral (length late))
      unless (null late) $ flow $ logInfo $ "sessionizer: " <> show (length late) <> " records took their state from Redis history (" <> show (M.size fallback) <> " found)"
      let stateAt did atMs = entryAt loaded did atMs <|> M.lookup (did, atMs) fallback
          withPings =
            foldl'
              ( \a p ->
                  let (atMs, entry) = placePing stateAt p
                   in addToRow (floorBucket atMs, p.pingCity, p.pingDriverId) entry mempty {pings = 1, online = True, lastPing = Just (atMs, p.pingLat, p.pingLon)} a
              )
              loaded
              pingsAt
      pure $
        foldl'
          (\a ev -> addToRow (floorBucket ev.ts, ev.merchantOperatingCityId, ev.driverId) (stateAt ev.driverId ev.ts) mempty {counts = eventCounts ev} a)
          withPings
          eventsAt

    lateStatesRetrying conn wanted = do
      res <- statesAt conn wanted `catchAny` (pure . Left . show)
      case res of
        Right found -> pure found
        Left err -> do
          flow $ logError $ "sessionizer: state history lookup failed, retrying: " <> err
          CC.threadDelay 1000000
          lateStatesRetrying conn wanted

    flush stateEnv idle now acc = do
      let rows = [analyticsRow k v | (k, v) <- M.toList acc.activity]
      produced <- produceRows appEnv.kafkaProducerTools.producer rows `catchAny` (pure . Left . show)
      case produced of
        Left err -> do
          flow $ logError $ "sessionizer: producing to " <> analyticsTopic <> " failed, will retry: " <> err
          P.incCounter acc.metrics.flushFailures
          pure acc {nextFlushMs = now + flushRetryMs}
        Right () -> do
          writeFlushedRetrying stateEnv.hedisConnection acc.offsets
          commitOffsets acc.offsets
          -- Flush progress (the integration tests wait on it): the oldest record
          -- time still to be flushed, or now when nothing is pending.
          let pending = M.elems acc.recordProgress <> [t | hr <- acc.heldRecords, Just (_, t) <- [heldDriverAt hr]]
              watermark = if idle || null pending then now else P.minimum pending
          flow (inStateRedis stateEnv (Hedis.set sessionizerWatermarkKey watermark)) `catchAny` \e ->
            flow $ logError $ "sessionizer: watermark update failed: " <> show e
          flow $ logInfo $ "sessionizer: flushed " <> show (length rows) <> " rows; " <> show (length acc.heldRecords) <> " records held (oldest for " <> show (maybe 0 (\hr -> now - hr.hrHeldAtMs) (listToMaybe acc.heldRecords)) <> " ms); " <> show (M.size acc.pendingLoad) <> " drivers waiting for the writer; " <> show (M.size acc.drivers) <> " drivers in memory"
          -- Driver state stays; its history keeps only what held records can
          -- still need.
          pure
            acc{activity = M.empty,
                offsets = M.empty,
                recordProgress = M.empty,
                flushedThrough = M.empty,
                drivers = M.map (\dm -> dm {dmHistory = trimBefore watermark dm.dmHistory}) acc.drivers,
                nextFlushMs = now + flushEveryMs
               }

    -- Exactly the applied records: held pings stay uncommitted.
    commitOffsets offs = unless (M.null offs) $ do
      commitErr <-
        Consumer.commitPartitionsOffsets Consumer.OffsetCommit kc $
          [Consumer.TopicPartition (Consumer.TopicName t) (Consumer.PartitionId p) (Consumer.PartitionOffset (lastOffset + 1)) | ((t, p), (_, lastOffset)) <- M.toList offs]
      whenJust commitErr $ \err ->
        unless (isRdKafkaError "RdKafkaRespErrNoOffset" err) $ flow $ logError $ "sessionizer: offset commit failed: " <> show err

-- | Applies state events to the drivers in memory and seeded. Those of any
-- other driver are queued and the driver is dropped from memory, to be loaded
-- (seeded by now, maybe) once the writer has applied them.
applyStateRecords :: Integer -> Acc -> [ConsumerRecordD] -> Acc
applyStateRecords now acc records = foldl' apply read' events
  where
    events = [ev | cr <- records, Just (ev :: DriverPoolStateEvent) <- [decodeValue cr], ev.driverId `notElem` ignoredDriverIds]
    read' = foldl' (\a cr -> a {stateNext = M.insert (Consumer.unPartitionId (Consumer.crPartition cr)) (Consumer.unOffset (Consumer.crOffset cr) + 1) a.stateNext}) acc records
    apply a ev = case M.lookup ev.driverId a.drivers of
      Just dm | dm.dmSeeded && M.notMember ev.driverId a.pendingLoad -> applyEvent a ev
      _ ->
        a
          { drivers = M.delete ev.driverId a.drivers,
            pendingLoad = M.insertWith queue ev.driverId (PendingLoad ev.ts now [ev]) a.pendingLoad
          }
    queue new old = old {plNeededMs = max old.plNeededMs new.plNeededMs, plEvents = new.plEvents <> old.plEvents}

-- | Applies one event as the writer does: merge the changes into the current
-- state, and add a history entry when the snapshot differs from the newest.
applyEvent :: Acc -> DriverPoolStateEvent -> Acc
applyEvent acc ev = case M.lookup ev.driverId acc.drivers of
  Nothing -> acc
  Just dm
    | ev.ts <= dm.dmLoadedThroughMs -> acc
    | otherwise ->
      let current = AKM.union (AKM.filterWithKey (\k _ -> S.member (AK.toText k) stateKeySet) (stampVehicleChange ev.ts dm.dmCurrent ev.changes)) dm.dmCurrent
          snapshot = snapshotOf current
          history = if (snd <$> M.lookupMax dm.dmHistory) == Just snapshot then dm.dmHistory else M.insert ev.ts snapshot dm.dmHistory
       in acc {drivers = M.insert ev.driverId dm {dmCurrent = current, dmHistory = history} acc.drivers}

-- | The history entry in force at @atMs@.
entryAt :: Acc -> Text -> Integer -> Maybe (Integer, DriverStateSnapshot)
entryAt acc did atMs = M.lookup did acc.drivers >>= M.lookupLE atMs . (.dmHistory)

isBufferedPing :: PingAt -> Bool
isBufferedPing p = p.pingStMs - p.pingTsMs > bufferedPingGapMs

-- | Where a ping counts: at its st and the state then, unless the phone recorded
-- it on a ride and sent it later, when it counts at its ts and the state then. A
-- driver with no network can't be offered a ride, but one on a ride is working.
-- A phone clock far behind lands outside the ride, so it falls back to st too.
placePing :: (Text -> Integer -> Maybe (Integer, DriverStateSnapshot)) -> PingAt -> (Integer, Maybe (Integer, DriverStateSnapshot))
placePing stateAt p = case stateAt p.pingDriverId p.pingTsMs of
  Just entry | isBufferedPing p, entryOnRide entry -> (p.pingTsMs, Just entry)
  _ -> (p.pingStMs, stateAt p.pingDriverId p.pingStMs)

entryOnRide :: (Integer, DriverStateSnapshot) -> Bool
entryOnRide (_, snapshot) = AKM.lookup "onRide" snapshot.state == Just (A.Bool True)

-- | Adds to the row of the state from a history entry (ts, snapshot), or to
-- the unknown-state row. An entry with no state columns counts as no state.
addToRow :: (Integer, Text, Text) -> Maybe (Integer, DriverStateSnapshot) -> Activity -> Acc -> Acc
addToRow (b, rowCity, did) mbEntry a acc = case mbEntry of
  Just (entryTs, snapshot)
    | not (AKM.null snapshot.state) -> acc {activity = M.insertWith (<>) (b, rowCity, did, Just (stateTag snapshot.state)) a {stateAtMs = entryTs} acc.activity}
  _ -> acc {activity = M.insertWith (<>) (b, rowCity, did, Nothing) a acc.activity}

trackOffset :: Acc -> ConsumerRecordD -> Acc
trackOffset acc cr =
  let o = Consumer.unOffset (Consumer.crOffset cr)
   in acc {offsets = M.insertWith (\(_, new) (old, _) -> (old, new)) (topicPartition cr) (o, o) acc.offsets}

topicPartition :: ConsumerRecordD -> (Text, Int)
topicPartition cr = (Consumer.unTopicName (Consumer.crTopic cr), Consumer.unPartitionId (Consumer.crPartition cr))

-- | Produces the rows, 'rowsPerMessage' JSONEachRow lines per message, and
-- waits until every message is delivered.
produceRows :: Producer.KafkaProducer -> [A.Value] -> IO (Either Text ())
produceRows _ [] = pure (Right ())
produceRows producer rows = do
  failures <- newIORef []
  let report = \case
        Producer.DeliverySuccess _ _ -> pure ()
        Producer.DeliveryFailure _ err -> atomicModifyIORef' failures (\fs -> (show err : fs, ()))
        Producer.NoMessageError err -> atomicModifyIORef' failures (\fs -> (show err : fs, ()))
  queued <- forM (inChunks rowsPerMessage rows) $ \chunk ->
    Producer.produceMessage' producer (message chunk) report
  Producer.flushProducer producer
  delivered <- readIORef failures
  pure $ case (Either.lefts queued, delivered) of
    ([], []) -> Right ()
    (immediate, failed) -> Left (show (length immediate + length failed) <> " messages not delivered, e.g. " <> T.take 500 (show (map (\(Producer.ImmediateError e) -> show e) immediate <> failed :: [Text])))
  where
    message chunk =
      Producer.ProducerRecord
        { Producer.prTopic = Producer.TopicName analyticsTopic,
          Producer.prPartition = Producer.UnassignedPartition,
          Producer.prKey = Nothing,
          Producer.prValue = Just (LBS.toStrict (LBS.intercalate "\n" (map A.encode chunk)))
        }

inChunks :: Int -> [a] -> [[a]]
inChunks n xs = case splitAt n xs of
  (chunk, []) -> [chunk | not (null chunk)]
  (chunk, rest) -> chunk : inChunks n rest

-- | sessionizer:flushed:* of each partition; -1 when absent.
loadFlushed :: R.Connection -> [(Text, Int)] -> IO (Either Text (M.Map (Text, Int) Int64))
loadFlushed conn tps = do
  replies <- R.runRedis conn (forM tps (\(t, p) -> R.get (TE.encodeUtf8 (flushedOffsetKey t p))))
  pure $ do
    values <- either (Left . show) Right (sequence replies)
    Right (M.fromList [(tp, fromMaybe (-1) (readMaybe . T.unpack . TE.decodeUtf8 =<< v)) | (tp, v) <- zip tps values])

-- | The last applied offset of each partition.
writeFlushed :: R.Connection -> M.Map (Text, Int) (Int64, Int64) -> IO (Either Text ())
writeFlushed conn offs = do
  replies <- R.runRedis conn $
    forM (M.toList offs) $ \((t, p), (_, lastOffset)) ->
      R.set (TE.encodeUtf8 (flushedOffsetKey t p)) (TE.encodeUtf8 (show lastOffset))
  pure (either (Left . show) (const (Right ())) (sequence replies))

holdRecord :: Integer -> ConsumerRecordD -> HeldRecord
holdRecord now cr =
  HeldRecord
    { hrHeldAtMs = now,
      hrRecord = cr,
      hrDecoded =
        if isTopic eventsTopic cr
          then mfilter (\ev -> ev.driverId `notElem` ignoredDriverIds) (decodeValue cr) <&> DecodedEvent
          else do
            did <- decodeKey cr
            lu :: LU.LocationUpdates <- decodeValue cr
            pure (DecodedPing did lu (utcToMs (fromMaybe lu.ts lu.st)))
    }

-- | (driver id, the record's time in epoch ms)
decodedAt :: Decoded -> (Text, Integer)
decodedAt = \case
  DecodedPing did _ st -> (did, st)
  DecodedEvent ev -> (ev.driverId, ev.ts)

heldDriverAt :: HeldRecord -> Maybe (Text, Integer)
heldDriverAt hr = decodedAt <$> hr.hrDecoded

-- | The writer watermark a driver can be loaded from Redis at, for a record
-- or event at @atMs@ read at @readAtMs@: 'stateBufferMs' past it (capped at
-- when it was read, so a clock running ahead waits only the buffer), and past
-- this consumer's start, after which this consumer reads every event itself.
writerTarget :: Integer -> Integer -> Integer -> Integer
writerTarget stateStartMs atMs readAtMs = max stateStartMs (min atMs readAtMs) + stateBufferMs

-- | An event's counts on the known columns, money in ten-thousandths.
eventCounts :: AnalyticsEvent -> M.Map Text Integer
eventCounts ev =
  M.filter
    (/= 0)
    ( M.fromList
        [ (column, if column `elem` moneyColumns then round (n * 10000) else round n)
          | column <- eventColumns,
            Just (A.Number n) <- [AKM.lookup (AK.fromText column) ev.counts]
        ]
    )

-- | Current state and history of each driver from the writer's Redis:
-- (state-column fields, seeded from the DB, history). Pipelined per 'loadChunk'.
loadDrivers :: R.Connection -> [Text] -> IO (Either Text (M.Map Text (A.Object, Bool, M.Map Integer DriverStateSnapshot)))
loadDrivers conn dids = do
  loaded <- forM (inChunks loadChunk dids) $ \chunk -> do
    replies <- R.runRedis conn $
      forM chunk $ \did -> do
        fields <- R.hgetall (TE.encodeUtf8 (currentKey did))
        members <- R.zrange (TE.encodeUtf8 (historyKey did)) 0 (-1)
        pure ((,) <$> fields <*> members)
    pure (either (Left . show) (Right . zip chunk) (sequence replies))
  pure (M.fromList . map (\(did, reply) -> (did, toMem reply)) . concat <$> sequence loaded)
  where
    toMem (fields, members) =
      let decoded = [(TE.decodeUtf8 field, v) | (field, raw) <- fields, Just v <- [A.decodeStrict raw]]
       in ( AKM.fromList [(AK.fromText field, v) | (field, v) <- decoded, S.member field stateKeySet],
            DL.lookup seededField decoded == Just (A.Bool True),
            M.fromList (mapMaybe decodeMember members)
          )

-- | Drops history entries a held ping can no longer need: those before the
-- newest entry at or before @cutoffMs@.
trimBefore :: Integer -> M.Map Integer DriverStateSnapshot -> M.Map Integer DriverStateSnapshot
trimBefore cutoffMs history = case M.lookupLE cutoffMs history of
  Just (keepFrom, _) -> snd (M.split (keepFrom - 1) history)
  Nothing -> history

-- | A held ping needs the state topic read up to this time: 'stateBufferMs'
-- past its st, capped at when it arrived so a clock running ahead does not
-- hold every later ping.
neededMs :: HeldRecord -> Integer
neededMs hr = maybe 0 (\(_, atMs) -> min atMs hr.hrHeldAtMs + stateBufferMs) (heldDriverAt hr)

probeReached :: Acc -> Probe -> Bool
probeReached acc pr = all (\(p, end) -> M.findWithDefault 0 p acc.stateNext >= end) (M.toList pr.probeEnds)

stateKeySet :: S.Set Text
stateKeySet = S.fromList stateColumnKeys

snapshotOf :: A.Object -> DriverStateSnapshot
snapshotOf current = snapshotFromFields stateColumnKeys (HM.fromList [(AK.toText k, v) | (k, v) <- AKM.toList current])

isTopic :: Text -> ConsumerRecordD -> Bool
isTopic t cr = Consumer.unTopicName (Consumer.crTopic cr) == t

-- | One driver_analytics_5m row: the presence, event and state columns.
-- Without a known state the state columns are omitted (NULL / []) and
-- state_id is 0.
analyticsRow :: ActivityKey -> Activity -> A.Value
analyticsRow (bucket, cityKey, did, mbStateTag) a =
  A.object $
    [ "bucket" A..= (bucket `div` 1000),
      "merchant_operating_city_id" A..= cityKey,
      "driver_id" A..= did,
      "pings" A..= a.pings,
      "online" A..= (if a.online then 1 else 0 :: Int),
      "state_id" A..= maybe 0 stateIdOf mbStateTag
    ]
      <> maybe [] (\(ts, lat, lon) -> ["last_ping_at" A..= epochMsText ts, "lat" A..= lat, "lon" A..= lon]) a.lastPing
      <> [AK.fromText column A..= (if column `elem` moneyColumns then A.Number (Sci.scientific n (-4)) else A.toJSON n) | (column, n) <- M.toList a.counts]
      <> stateFields
  where
    stateFields = case mbStateTag of
      Nothing -> ["state_known" A..= (0 :: Int)]
      Just tagText -> ["state_known" A..= (1 :: Int), "state_at" A..= epochMsText a.stateAtMs] <> renderStateValues (stateOfTag tagText)

floorBucket :: Integer -> Integer
floorBucket t = t - t `mod` bucketMs

utcToMs :: UTCTime -> Integer
utcToMs u = round (utcTimeToPOSIXSeconds u * 1000)

nowMs :: IO Integer
nowMs = round . (* 1000) <$> getPOSIXTime

decodeValue :: FromJSON a => ConsumerRecordD -> Maybe a
decodeValue cr = A.decode . LBS.fromStrict =<< Consumer.crValue cr

decodeKey :: ConsumerRecordD -> Maybe Text
decodeKey cr = TE.decodeUtf8 <$> Consumer.crKey cr

inStateRedis :: Hedis.HedisEnv -> Flow a -> Flow a
inStateRedis stateEnv = local (Hedis.withSecondaryRedisEnv stateEnv)
