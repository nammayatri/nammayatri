-- | SESSIONIZER: turns LTS pings (location-updates) into 5-minute per-driver
-- online time in ClickHouse. The interval between two consecutive pings of a
-- driver counts as online when the earlier ping's mode is ONLINE or SILENT and
-- the gap is at most 'maxGapMs'. The interval is split at every state change
-- the DRIVER_POOL_STATE writer recorded inside it, each piece going to the
-- row of its state, and split at bucket edges.
--
-- It goes to atlas_kafka.driver_analytics_5m, one row per (bucket, city,
-- driver, driver state), activity columns only: DRIVER_PERFORMANCE writes the
-- performance_analytics columns of the same rows. Each online piece goes to
-- the row of its segment's state, each ping to the row of the state at its
-- own time. The table is a SummingMergeTree, so rows for the same key from
-- different flushes add up.
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
import qualified Data.Aeson.KeyMap as AKM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Either as Either
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
import Processor.ClickhouseText (asciiJson)
import Processor.DriverPoolState.Types
import qualified Processor.LocationUpdate.Types as LU
import Processor.StateColumns (renderStateValues, stateOfTag, stateTag, statesAt)
import Transporter.Kafka.Committing (isRdKafkaError)
import qualified Prelude as P

locationTopic :: Text
locationTopic = "location-updates"

bucketMs, maxGapMs, flushEveryMs, flushRetryMs :: Integer
bucketMs = 5 * 60 * 1000
maxGapMs = 300 * 1000
flushEveryMs = 5 * 60 * 1000
flushRetryMs = 30 * 1000

-- | Pings stamped more than this far ahead of the wall clock, or before
-- 'minPlausiblePingMs' (2020-01-01), are skipped: one future ping would
-- otherwise become the driver's last ping and block every later one, and push
-- the sessionizer watermark (so the writer's history trimming) into the future.
maxFutureSkewMs, minPlausiblePingMs :: Integer
maxFutureSkewMs = 5 * 60 * 1000
minPlausiblePingMs = 1577836800000

pollBatchSize :: Int
pollBatchSize = 1000

data Ping = Ping
  { stMs :: Integer,
    online :: Bool,
    city :: Text
  }

-- | A ping waiting for the state at its own time.
data PingAt = PingAt
  { pingDriverId :: Text,
    pingStMs :: Integer,
    pingCity :: Text
  }

-- | Measures for one row. Durations in milliseconds.
data Activity = Activity
  { onlineMs :: Integer,
    onRideMs :: Integer,
    pings :: Integer,
    pingsUnknownState :: Integer,
    -- | state_at: the ts of the history entry the row's state comes from
    stateAtMs :: Integer
  }

instance Semigroup Activity where
  a <> b =
    Activity
      { onlineMs = a.onlineMs + b.onlineMs,
        onRideMs = a.onRideMs + b.onRideMs,
        pings = a.pings + b.pings,
        pingsUnknownState = a.pingsUnknownState + b.pingsUnknownState,
        stateAtMs = max a.stateAtMs b.stateAtMs
      }

instance Monoid Activity where
  mempty = Activity 0 0 0 0 0

-- | (bucket start ms, city, driver, the driver's state as a 'stateTag';
-- Nothing when it is unknown)
type ActivityKey = (Integer, Text, Text, Maybe Text)

data Acc = Acc
  { lastPing :: M.Map Text Ping,
    activity :: M.Map ActivityKey Activity,
    -- | Last ping time handled per location partition since the last flush.
    pingProgress :: M.Map Int Integer,
    -- | First and last offset per (topic, partition) since the last flush.
    offsets :: M.Map (Text, Int) (Int64, Int64),
    nextFlushMs :: Integer
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
  pollLoop stateEnv Acc {lastPing = M.empty, activity = M.empty, pingProgress = M.empty, offsets = M.empty, nextFlushMs = start + flushEveryMs}
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
      batchNow <- nowMs
      let withOffsets = foldl' trackOffset acc records
          (withPings, pingsAt, credits) = foldl' (takePing batchNow) (withOffsets, [], []) (filter isLocation records)
      -- Credits look up states from their start, pings at their own time: wait,
      -- however long it takes, until the writer is 'stateBufferMs' past both on
      -- every partition, so every state change up to the batch's newest ping is applied.
      let neededMs = map (\c -> c.fromMs) credits <> map (\p -> p.pingStMs) pingsAt
      wm <- if null neededMs then pure 0 else awaitStateWriter (flow . logWarning . ("sessionizer: " <>)) stateEnv.hedisConnection (maximum neededMs)
      withCredits <-
        if null credits
          then pure withPings
          else foldl' applyCredit withPings <$> lookupStates stateEnv wm credits
      -- After the writer wait: one pipelined lookup per 1000 pings of the batch.
      pingStates <- pingStatesRetrying stateEnv [(p.pingDriverId, p.pingStMs) | p <- pingsAt]
      pure $ foldl' (\acc' p -> addToRow (floorBucket p.pingStMs, p.pingCity, p.pingDriverId) (M.lookup (p.pingDriverId, p.pingStMs) pingStates) mempty {pings = 1} acc') withCredits pingsAt
      where
        isLocation cr = Consumer.unTopicName (Consumer.crTopic cr) == locationTopic

    trackOffset acc cr =
      let k = (Consumer.unTopicName (Consumer.crTopic cr), Consumer.unPartitionId (Consumer.crPartition cr))
          o = Consumer.unOffset (Consumer.crOffset cr)
       in acc {offsets = M.insertWith (\(_, new) (old, _) -> (old, new)) k (o, o) acc.offsets}

    -- The ping is counted once its state is looked up; the interval from the
    -- driver's previous ping becomes a Credit when that ping was online.
    takePing batchNow (acc, pingsAt, credits) cr = case (decodeKey cr, decodeValue cr) of
      (Just did, Just (lu :: LU.LocationUpdates))
        | plausible (utcToMs (fromMaybe lu.ts lu.st)) ->
          let st = utcToMs (fromMaybe lu.ts lu.st)
              cur = Ping {stMs = st, online = lu.mode `elem` [Just "ONLINE", Just "SILENT"], city = fromMaybe "" lu.mocid}
              prev = M.lookup did acc.lastPing
              credits' = case prev of
                Just p | p.online && st > p.stMs && st - p.stMs <= maxGapMs -> Credit {driverId = did, fromMs = p.stMs, toMs = st, city = p.city} : credits
                _ -> credits
              newer = maybe True (\p -> st >= p.stMs) prev
              acc' =
                acc{lastPing = if newer then M.insert did cur acc.lastPing else acc.lastPing,
                    pingProgress = M.insert (Consumer.unPartitionId (Consumer.crPartition cr)) st acc.pingProgress
                   }
           in (acc', PingAt {pingDriverId = did, pingStMs = st, pingCity = cur.city} : pingsAt, credits')
      _ -> (acc, pingsAt, credits)
      where
        plausible st = st >= minPlausiblePingMs && st <= batchNow + maxFutureSkewMs

    -- Each segment is credited to the row of its own state; the ping counts
    -- as unknown when the state at its own time (the last segment's) is
    -- unknown, in the unknown-state row.
    applyCredit acc (c, rawSegments) =
      let knownEntry mbEntry = case mbEntry of
            Just (_, s) | not (AKM.null s.state) -> mbEntry
            _ -> Nothing
          -- An entry written before snapshots carried @state@ counts as no state.
          segments = [(segFrom, segTo, knownEntry mbEntry) | (segFrom, segTo, mbEntry) <- rawSegments]
          classify mbEntry ms = case snd <$> mbEntry of
            Just s | s.onRide -> mempty {onlineMs = ms, onRideMs = ms}
            _ -> mempty {onlineMs = ms}
          creditSegment accSeg (segFrom, segTo, mbEntry) =
            foldl' (\accPiece (b, ms) -> addToRow (b, c.city, c.driverId) mbEntry (classify mbEntry ms) accPiece) accSeg (splitAtBuckets segFrom segTo)
          withTime = foldl' creditSegment acc segments
          endsUnknown = maybe True (\(_, _, s) -> isNothing s) (listToMaybe (reverse segments))
       in if endsUnknown then addToRow (floorBucket c.toMs, c.city, c.driverId) (Nothing :: Maybe (Integer, DriverStateSnapshot)) mempty {pingsUnknownState = 1} withTime else withTime

    -- Adds to the row of the state from a history entry (ts, snapshot), or
    -- to the unknown-state row.
    -- An entry written before snapshots carried @state@ counts as no state.
    addToRow (b, rowCity, did) mbEntry a acc = case mbEntry of
      Just (entryTs, snapshot)
        | not (AKM.null snapshot.state) -> acc {activity = M.insertWith (<>) (b, rowCity, did, Just (stateTag snapshot.state)) a {stateAtMs = entryTs} acc.activity}
      _ -> acc {activity = M.insertWith (<>) (b, rowCity, did, Nothing) a acc.activity}

    -- A failed read is retried every second, like DRIVER_PERFORMANCE's state
    -- reads, rather than counting the batch's pings in the unknown-state row.
    pingStatesRetrying stateEnv wanted = do
      res <- statesAt stateEnv.hedisConnection wanted `catchAny` (pure . Left . show)
      case res of
        Right found -> pure found
        Left err -> do
          flow $ logError $ "sessionizer: ping state lookup failed, retrying: " <> err
          CC.threadDelay 1000000
          pingStatesRetrying stateEnv wanted

    -- One pipelined round-trip: per credit, the newest history entry at or
    -- before its start plus every change inside it, turned into
    -- (from, to, (entry ts, state)) segments covering the credit. The writer
    -- is past every credit's start; only a ping stamped ahead of the clock can
    -- start after its watermark, and is then one unknown segment.
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
          let starts = (c.fromMs, listToMaybe (decoded atStart)) : [(t, Just entry) | entry@(t, _) <- decoded inside]
              ends = drop 1 (map fst starts) <> [c.toMs]
           in zipWith (\(segFrom, s) segTo -> (segFrom, segTo, s)) starts ends

    flush stateEnv idle now acc = do
      let token = offsetsToken acc.offsets
          rows = [analyticsRow k v | (k, v) <- M.toList acc.activity]
      ok <- insertRows "driver_analytics_5m" token rows
      if not ok
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
          flow $ logInfo $ "sessionizer: flushed " <> show (length rows) <> " activity rows"
          pure
            acc{activity = M.empty,
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
          query = "INSERT INTO " <> table <> " SETTINGS insert_deduplication_token = '" <> token <> "', date_time_input_format = 'best_effort' FORMAT JSONEachRow\n" <> body
      res <- flow (runExecQuery' (ExprStr (T.unpack (asciiJson query))) ATLAS_KAFKA) `catchAny` \e -> pure (Left (P.show e))
      case res of
        Right _ -> pure True
        Left err -> do
          flow $ logError $ "sessionizer: insert into " <> table <> " failed, will retry: " <> T.pack err
          pure False

-- | One driver_analytics_5m row: the activity columns and the state columns
-- (DRIVER_PERFORMANCE writes the others). Without a known state they are
-- omitted (NULL / []).
analyticsRow :: ActivityKey -> Activity -> A.Value
analyticsRow (bucket, cityKey, did, mbStateTag) a =
  A.object $
    [ "bucket" A..= (bucket `div` 1000),
      "merchant_operating_city_id" A..= cityKey,
      "driver_id" A..= did,
      "online_ms" A..= a.onlineMs,
      "online_on_ride_ms" A..= a.onRideMs,
      "pings" A..= a.pings,
      "pings_unknown_state" A..= a.pingsUnknownState
    ]
      <> stateFields
  where
    stateFields = case mbStateTag of
      Nothing -> ["state_known" A..= (0 :: Int)]
      Just tagText -> ["state_known" A..= (1 :: Int), "state_at" A..= a.stateAtMs] <> renderStateValues (stateOfTag tagText)

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
