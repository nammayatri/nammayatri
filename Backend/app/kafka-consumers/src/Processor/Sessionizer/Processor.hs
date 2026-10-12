-- | SESSIONIZER: turns LTS pings and app analytics events into 5-minute
-- per-driver rows in ClickHouse.
--
--   * location-updates: online time. The interval between two consecutive pings
--     of a driver counts as online when the earlier ping's mode is ONLINE or
--     SILENT and the gap is at most 'maxGapMs'. The interval is split at every
--     state change the DRIVER_POOL_STATE writer recorded inside it, each piece
--     tagged with its state (on ride / idle / unavailable / unknown), and split
--     at bucket edges.
--   * dynamic-offer-driver-events-updates: every event counted per bucket,
--     city, driver (person_id), event_type and sub_type.
--
-- Every 'flushEveryMs' the counts are inserted (with a dedup token), then the
-- offsets are committed, then sessionizer:watermark is published for the
-- writer's history trimming. A crash before the commit replays the window.
module Processor.Sessionizer.Processor
  ( runSessionizer,
  )
where

import qualified Control.Concurrent as CC
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Either as Either
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import qualified Database.Redis as R
import Environment
import qualified EulerHS.Runtime as L
import qualified Kafka.Consumer as Consumer
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseDb (ATLAS_KAFKA))
import Kernel.Storage.Clickhouse.Queries (runExecQuery')
import Kernel.Storage.Clickhouse.Types (ClickhouseExpr (ExprStr))
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Flow
import Kernel.Utils.Common hiding (id)
import Processor.DriverPoolState.Types
import qualified Processor.LocationUpdate.Types as LU
import Transporter.Kafka.Committing (isRdKafkaError)
import qualified Prelude as P

locationTopic :: Text
locationTopic = "location-updates"

bucketMs, maxGapMs, flushEveryMs, flushRetryMs, writerWaitTimeoutMs :: Integer
bucketMs = 5 * 60 * 1000
maxGapMs = 300 * 1000
flushEveryMs = 5 * 60 * 1000
flushRetryMs = 30 * 1000
writerWaitTimeoutMs = 30 * 1000

pollBatchSize :: Int
pollBatchSize = 1000

-- | One analytics event, as published by Lib.SessionizerMetrics (snake_case JSON).
data SessionEvent = SessionEvent
  { atMs :: Double,
    eventType :: Text,
    subType :: Maybe Text,
    personId :: Maybe Text,
    cityId :: Maybe Text
  }

instance FromJSON SessionEvent where
  parseJSON = A.withObject "SessionEvent" $ \o ->
    SessionEvent
      <$> o A..: "at"
      <*> o A..: "event_type"
      <*> o A..:? "sub_type"
      <*> o A..:? "person_id"
      <*> o A..:? "merchant_operating_city_id"

data Ping = Ping
  { stMs :: Integer,
    online :: Bool,
    city :: Text
  }

-- | Measures for one (bucket, city, driver) row. Durations in milliseconds.
data Activity = Activity
  { onlineMs :: Integer,
    onRideMs :: Integer,
    idleMs :: Integer,
    unavailableMs :: Integer,
    unknownStateMs :: Integer,
    pings :: Integer,
    pingsUnknownState :: Integer
  }

instance Semigroup Activity where
  a <> b =
    Activity
      { onlineMs = a.onlineMs + b.onlineMs,
        onRideMs = a.onRideMs + b.onRideMs,
        idleMs = a.idleMs + b.idleMs,
        unavailableMs = a.unavailableMs + b.unavailableMs,
        unknownStateMs = a.unknownStateMs + b.unknownStateMs,
        pings = a.pings + b.pings,
        pingsUnknownState = a.pingsUnknownState + b.pingsUnknownState
      }

instance Monoid Activity where
  mempty = Activity 0 0 0 0 0 0 0

-- | (bucket start ms, city, driver)
type ActivityKey = (Integer, Text, Text)

-- | (bucket start ms, city, driver, event_type, sub_type)
type EventKey = (Integer, Text, Text, Text, Text)

data Acc = Acc
  { lastPing :: M.Map Text Ping,
    activity :: M.Map ActivityKey Activity,
    events :: M.Map EventKey Integer,
    -- | Last ping time handled per location partition since the last flush.
    pingProgress :: M.Map Int Integer,
    -- | First and last offset per (topic, partition) since the last flush.
    offsets :: M.Map (Text, Int) (Int64, Int64),
    nextFlushMs :: Integer,
    writerWatermark :: Integer
  }

-- | An online interval waiting for its state lookup.
data Credit = Credit
  { driverId :: Text,
    fromMs :: Integer,
    toMs :: Integer,
    city :: Text
  }

runSessionizer :: L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
runSessionizer flowRt appEnv kc = do
  stateEnv <- maybe (P.error "driverStateRedisCfg is not set; SESSIONIZER needs it") pure appEnv.driverStateHedisEnv
  start <- nowMs
  pollLoop stateEnv Acc {lastPing = M.empty, activity = M.empty, events = M.empty, pingProgress = M.empty, offsets = M.empty, nextFlushMs = start + flushEveryMs, writerWatermark = 0}
  where
    flow :: Flow a -> IO a
    flow = runFlowR flowRt appEnv

    pollLoop stateEnv acc = do
      polled <- Consumer.pollMessageBatch kc (Consumer.Timeout 500) (Consumer.BatchSize pollBatchSize)
      forM_ (Either.lefts polled) $ \err ->
        unless (isRdKafkaError "RdKafkaRespErrTimedOut" err) $ flow $ logError $ "sessionizer: kafka poll error: " <> show err
      let records = Either.rights polled
      acc' <- processBatch stateEnv acc records
      now <- nowMs
      acc'' <- if now >= acc'.nextFlushMs then flush stateEnv (null records) now acc' else pure acc'
      pollLoop stateEnv acc''

    processBatch stateEnv acc records = do
      let withOffsets = foldl' trackOffset acc records
          (locRecords, eventRecords) = DL.partition isLocation records
          withEvents = foldl' countEvent withOffsets eventRecords
          (withPings, credits) = foldl' takePing (withEvents, []) locRecords
      if null credits
        then pure withPings
        else do
          let needed = maximum (map (\c -> c.fromMs) credits)
          wm <- waitForWriter stateEnv withPings.writerWatermark needed
          states <- lookupStates stateEnv wm credits
          pure $ foldl' applyCredit withPings {writerWatermark = wm} states
      where
        isLocation cr = Consumer.unTopicName (Consumer.crTopic cr) == locationTopic

    trackOffset acc cr =
      let k = (Consumer.unTopicName (Consumer.crTopic cr), Consumer.unPartitionId (Consumer.crPartition cr))
          o = Consumer.unOffset (Consumer.crOffset cr)
       in acc {offsets = M.insertWith (\(_, new) (old, _) -> (old, new)) k (o, o) acc.offsets}

    countEvent acc cr = case decodeValue cr of
      Nothing -> acc
      Just (ev :: SessionEvent) ->
        let k = (floorBucket (round ev.atMs), fromMaybe "" ev.cityId, fromMaybe "" ev.personId, ev.eventType, fromMaybe "" ev.subType)
         in acc {events = M.insertWith (+) k 1 acc.events}

    -- Ping count goes to the ping's own bucket; the interval from the driver's
    -- previous ping becomes a Credit when that ping was online.
    takePing (acc, credits) cr = case (decodeKey cr, decodeValue cr) of
      (Just did, Just (lu :: LU.LocationUpdates)) ->
        let st = utcToMs (fromMaybe lu.ts lu.st)
            cur = Ping {stMs = st, online = lu.mode `elem` [Just "ONLINE", Just "SILENT"], city = fromMaybe "" lu.mocid}
            prev = M.lookup did acc.lastPing
            credits' = case prev of
              Just p | p.online && st > p.stMs && st - p.stMs <= maxGapMs -> Credit {driverId = did, fromMs = p.stMs, toMs = st, city = p.city} : credits
              _ -> credits
            newer = maybe True (\p -> st >= p.stMs) prev
            acc' =
              acc{lastPing = if newer then M.insert did cur acc.lastPing else acc.lastPing,
                  activity = M.insertWith (<>) (floorBucket st, cur.city, did) mempty {pings = 1} acc.activity,
                  pingProgress = M.insert (Consumer.unPartitionId (Consumer.crPartition cr)) st acc.pingProgress
                 }
         in (acc', credits')
      _ -> (acc, credits)

    -- Each segment is credited to its own state; the ping counts as unknown
    -- when the state at its own time (the last segment's) is unknown.
    applyCredit acc (c, segments) =
      let classify mbState ms = case mbState of
            Nothing -> mempty {onlineMs = ms, unknownStateMs = ms}
            Just s
              | s.onRide -> mempty {onlineMs = ms, onRideMs = ms}
              | not s.enabled || s.blocked -> mempty {onlineMs = ms, unavailableMs = ms}
              | otherwise -> mempty {onlineMs = ms, idleMs = ms}
          creditSegment m (segFrom, segTo, mbState) =
            foldl' (\m' (b, ms) -> M.insertWith (<>) (b, c.city, c.driverId) (classify mbState ms) m') m (splitAtBuckets segFrom segTo)
          withTime = foldl' creditSegment acc.activity segments
          endsUnknown = maybe True (\(_, _, s) -> isNothing s) (listToMaybe (reverse segments))
          unknownPing = if endsUnknown then M.insertWith (<>) (floorBucket c.toMs, c.city, c.driverId) mempty {pingsUnknownState = 1} withTime else withTime
       in acc {activity = unknownPing}

    -- Block until the writer has applied events up to @needed@, or give up
    -- after 'writerWaitTimeoutMs' (those intervals are then tagged unknown).
    waitForWriter stateEnv cached needed
      | needed <= cached = pure cached
      | otherwise = go 0
      where
        go waited = do
          wm <- fromMaybe 0 <$> flow (inStateRedis stateEnv (Hedis.get writerWatermarkKey))
          if wm >= needed || waited >= writerWaitTimeoutMs
            then pure wm
            else CC.threadDelay 200000 >> go (waited + 200)

    -- One pipelined round-trip: per credit, the newest history entry at or
    -- before its start plus every change inside it, turned into
    -- (from, to, state) segments covering the credit. Credits starting after
    -- the writer watermark are one unknown segment. Changes after the
    -- watermark are not seen yet, so the tail past it keeps the last known
    -- state (bounded by the writer's lag).
    lookupStates stateEnv wm credits = do
      let ordered = reverse credits
      replies <-
        R.runRedis stateEnv.hedisConnection $
          forM ordered $ \c -> do
            let hk = TE.encodeUtf8 (historyKey c.driverId)
            atStart <- R.zrevrangebyscoreLimit hk (fromIntegral c.fromMs) (-1 / 0) 0 1
            inside <- R.zrangebyscore hk (fromIntegral c.fromMs + 0.5) (min (fromIntegral c.toMs - 0.5) (fromIntegral wm))
            pure (atStart, inside)
      pure
        [ (c, if c.fromMs > wm then [(c.fromMs, c.toMs, Nothing)] else segments c atStart inside)
          | (c, (atStart, inside)) <- zip ordered replies
        ]
      where
        decoded = either (const []) (mapMaybe decodeMember)
        segments c atStart inside =
          let starts = (c.fromMs, snd <$> listToMaybe (decoded atStart)) : [(t, Just s) | (t, s) <- decoded inside]
              ends = drop 1 (map fst starts) <> [c.toMs]
           in zipWith (\(segFrom, s) segTo -> (segFrom, segTo, s)) starts ends

    flush stateEnv idle now acc = do
      let token = offsetsToken acc.offsets
          activityRows = [activityRow k v | (k, v) <- M.toList acc.activity]
          eventRows = [eventRow k n | (k, n) <- M.toList acc.events]
      okA <- insertRows "driver_activity_5m" (token <> "-a") activityRows
      okE <- if okA then insertRows "driver_events_5m" (token <> "-e") eventRows else pure False
      if not (okA && okE)
        then pure acc {nextFlushMs = now + flushRetryMs}
        else do
          commitErr <- Consumer.commitAllOffsets Consumer.OffsetCommit kc
          whenJust commitErr $ \err ->
            unless (isRdKafkaError "RdKafkaRespErrNoOffset" err) $ flow $ logError $ "sessionizer: offset commit failed: " <> show err
          -- Pulled back by maxGapMs: a later interval can start at a ping up to
          -- that much older, and the writer must keep the state at its start.
          let watermark = (if idle || M.null acc.pingProgress then now else minimum (M.elems acc.pingProgress)) - maxGapMs
          flow (inStateRedis stateEnv (Hedis.set sessionizerWatermarkKey watermark)) `catchAny` \e ->
            flow $ logError $ "sessionizer: watermark update failed: " <> show e
          flow $ logInfo $ "sessionizer: flushed " <> show (length activityRows) <> " activity rows, " <> show (length eventRows) <> " event rows"
          pure
            acc{activity = M.empty,
                events = M.empty,
                offsets = M.empty,
                pingProgress = M.empty,
                -- Forget drivers silent for longer than any creditable gap.
                lastPing = M.filter (\p -> now - p.stMs <= 2 * maxGapMs) acc.lastPing,
                nextFlushMs = now + flushEveryMs
               }

    insertRows :: Text -> Text -> [A.Value] -> IO Bool
    insertRows _ _ [] = pure True
    insertRows table token rows = do
      let body = T.intercalate "\n" (map (TE.decodeUtf8 . LBS.toStrict . A.encode) rows)
          query = "INSERT INTO " <> table <> " SETTINGS insert_deduplication_token = '" <> token <> "' FORMAT JSONEachRow\n" <> body
      res <- flow (runExecQuery' (ExprStr (T.unpack query)) ATLAS_KAFKA) `catchAny` \e -> pure (Left (P.show e))
      case res of
        Right _ -> pure True
        Left err -> do
          flow $ logError $ "sessionizer: insert into " <> table <> " failed, will retry: " <> T.pack err
          pure False

activityRow :: ActivityKey -> Activity -> A.Value
activityRow (bucket, cityKey, did) a =
  A.object
    [ "bucket" A..= (bucket `div` 1000),
      "merchant_operating_city_id" A..= cityKey,
      "driver_id" A..= did,
      "online_ms" A..= a.onlineMs,
      "online_on_ride_ms" A..= a.onRideMs,
      "online_idle_ms" A..= a.idleMs,
      "online_unavailable_ms" A..= a.unavailableMs,
      "online_unknown_state_ms" A..= a.unknownStateMs,
      "pings" A..= a.pings,
      "pings_unknown_state" A..= a.pingsUnknownState
    ]

eventRow :: EventKey -> Integer -> A.Value
eventRow (bucket, cityKey, did, evType, evSubType) n =
  A.object
    [ "bucket" A..= (bucket `div` 1000),
      "merchant_operating_city_id" A..= cityKey,
      "driver_id" A..= did,
      "event_type" A..= evType,
      "sub_type" A..= evSubType,
      "events" A..= n
    ]

-- | Same offsets → same token, so ClickHouse drops a re-sent identical batch.
offsetsToken :: M.Map (Text, Int) (Int64, Int64) -> Text
offsetsToken m =
  "sessionizer:" <> T.intercalate "," [t <> "/" <> show p <> "/" <> show a <> "-" <> show b | ((t, p), (a, b)) <- M.toList m]

-- | Split [from, to) at bucket edges into (bucket start, ms) pieces.
splitAtBuckets :: Integer -> Integer -> [(Integer, Integer)]
splitAtBuckets startMs endMs
  | startMs >= endMs = []
  | otherwise =
    let b = floorBucket startMs
        pieceEnd = min endMs (b + bucketMs)
     in (b, pieceEnd - startMs) : splitAtBuckets pieceEnd endMs

floorBucket :: Integer -> Integer
floorBucket t = t - t `mod` bucketMs

utcToMs :: UTCTime -> Integer
utcToMs u = round (utcTimeToPOSIXSeconds u * 1000)

nowMs :: IO Integer
nowMs = round . (* 1000) <$> getPOSIXTime

decodeValue :: FromJSON a => Consumer.ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> Maybe a
decodeValue cr = A.decode . LBS.fromStrict =<< Consumer.crValue cr

decodeKey :: Consumer.ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> Maybe Text
decodeKey cr = TE.decodeUtf8 <$> Consumer.crKey cr

inStateRedis :: Hedis.HedisEnv -> Flow a -> Flow a
inStateRedis stateEnv = local (Hedis.withSecondaryRedisEnv stateEnv)
