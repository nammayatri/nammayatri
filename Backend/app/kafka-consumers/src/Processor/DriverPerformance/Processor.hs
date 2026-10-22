-- | DRIVER_PERFORMANCE: performance_analytics metrics from the drainers' CDC
-- topics (adob-sessionizer-*), into the atlas_kafka.driver_analytics_5m rows
-- the SESSIONIZER fills with online time.
--
-- Every pop-up and ride keeps in Redis the contribution it last emitted; a
-- new version emits new minus old, so a redelivery emits nothing and a pop-up
-- that becomes Pulled is retracted. Facts joined to a ride (booking,
-- cancellation reason, fare parameters, ratings) wait under their own key
-- until their ride is seen; a fact for a known ride recomputes it.
--
-- Rows are keyed by the driver's state at the entity's own time (pop-up and
-- ride created_at), looked up in the writer's history when the entity is
-- first seen and kept with its state, so every later version and retraction
-- lands in the same row. Joined facts count in their ride's row. Before
-- looking states up a batch waits (bounded) for the writer to reach its
-- newest event time, and every flush publishes
-- perf:state_horizon so the writer keeps the history those lookups need.
--
-- State lives in the writer's Redis (driverStateRedisCfg, keys perf:*). A poll
-- batch reads it in two pipelined round trips, plus one for the states of the
-- entities it sees first, and writes stay in memory.
-- Every 'flushEveryMs': (1) insert the deltas with a token derived from the
-- window's offsets, (2) only then write the state, (3) then commit exactly
-- those offsets, (4) start a new window and set perf:watermark. Nothing is
-- polled until a flush succeeds, so a retried insert is identical and
-- ClickHouse drops it if the first one landed. The offsets are recorded in
-- Redis before the insert: after a crash before the commit, the first flush
-- replays exactly those offsets against the pre-flush state with the same
-- token. A crash after (2) replays against the new state: zero deltas.
--
-- Run a single instance: an entity's state and the facts joined to it must be
-- handled by one process.
module Processor.DriverPerformance.Processor
  ( runDriverPerformance,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Either as Either
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Database.Redis as R
import Environment (AppEnv, ConsumerRecordD, Flow)
import qualified EulerHS.Runtime as L
import qualified Kafka.Consumer as Consumer
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseDb (ATLAS_KAFKA))
import Kernel.Storage.Clickhouse.Queries (runExecQuery')
import Kernel.Storage.Clickhouse.Types (ClickhouseExpr (ExprStr))
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.Common (logDebug, logError, logInfo, logWarning)
import Processor.ClickhouseText (asciiJson)
import Processor.DriverPerformance.Contribution
import Processor.DriverPerformance.Types
import Processor.DriverPoolState.Types (stateHorizonKey, writerWatermarkKey)
import Processor.StateColumns (renderStateValues, stateOfTag, stateTag, statesAt)
import Transporter.Kafka.Committing (isRdKafkaError)
import qualified Prelude as P

pollBatchSize :: Int
pollBatchSize = 1000

flushEveryMs :: Integer
flushEveryMs = 60 * 1000

-- | A failed flush step is retried after 30 s, a failed state read after 1 s.
flushRetryMicros, loadRetryMicros :: Int
flushRetryMicros = 30 * 1000000
loadRetryMicros = 1000000

-- | Keys per MGET, commands per pipelined write.
redisChunk :: Int
redisChunk = 500

-- | The bounded wait for the state writer before stamp lookups, like the
-- sessionizer's: poll every 200 ms, give up after 30 s.
writerPollMicros :: Int
writerPollMicros = 200000

writerWaitTimeoutMs :: Integer
writerWaitTimeoutMs = 30 * 1000

-- | perf:state_horizon is this far before the earliest event time looked up
-- in the window, for lookups of entities that arrive late.
horizonMarginMs :: Integer
horizonMarginMs = 10 * 60 * 1000

-- | perf:state_horizon expires, so a stopped consumer stops holding back the
-- writer's history trimming.
horizonTtl :: Integer
horizonTtl = 60 * 60

-- | How often a replay that has not reached every recorded offset logs an error.
replayAlertEveryMs :: Integer
replayAlertEveryMs = 5 * 60 * 1000

-- | No message within the timeout; a topic nothing has produced to yet
-- (matches UnknownTopicOrPart too).
quietPollErrors :: [Text]
quietPollErrors = ["RdKafkaRespErrTimedOut", "RdKafkaRespErrUnknownTopic"]

-- | Everything read, written and emitted since the last flush.
data Window = Window
  { -- | read from Redis (Nothing: absent)
    wLoaded :: !(M.Map Text (Maybe A.Value)),
    -- | written, with the TTL in seconds
    wDirty :: !(M.Map Text (A.Value, Integer)),
    wDeltas :: !(M.Map StampedKey Measures),
    -- | first and last applied offset per (topic, partition)
    wOffsets :: !(M.Map (Text, Int) (Int64, Int64)),
    -- | debug lines from applying the batch, newest first
    wNotes :: [Text],
    -- | the earliest event time (epoch ms) of the window's stamp lookups
    wMinLookupMs :: Maybe Integer
  }

emptyWindow :: Window
emptyWindow = Window {wLoaded = M.empty, wDirty = M.empty, wDeltas = M.empty, wOffsets = M.empty, wNotes = [], wMinLookupMs = Nothing}

data Loop = Loop
  { lpWindow :: Window,
    lpNextFlushMs :: Integer,
    lpReplay :: Maybe Replay
  }

-- | Replaying a flush that recorded its offsets but did not commit them.
data Replay = Replay
  { replayToken :: Text,
    -- | partitions not yet consumed up to the recorded last offset
    replayEnds :: M.Map (Text, Int) Int64,
    -- | partitions the group never committed, still to be sought back to the
    -- recorded first offset once assigned
    replaySeeks :: M.Map (Text, Int) Int64,
    -- | records past the recorded offsets, newest first, applied after the replayed flush
    replayHeld :: [ConsumerRecordD],
    -- | when a replay still waiting logs its next error
    replayAlertAtMs :: Integer
  }

runDriverPerformance :: L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
runDriverPerformance flowRt appEnv kc = do
  stateEnv <- maybe (P.error "driverStateRedisCfg is not set; DRIVER_PERFORMANCE needs it") pure appEnv.driverStateHedisEnv
  let conn = stateEnv.hedisConnection
  replay <- loadReplay conn
  start <- nowMs
  pollLoop conn Loop {lpWindow = emptyWindow, lpNextFlushMs = start + flushEveryMs, lpReplay = replay}
  where
    flow :: Flow a -> IO a
    flow = runFlowR flowRt appEnv

    pollLoop :: R.Connection -> Loop -> IO ()
    pollLoop conn lp = do
      polled <- Consumer.pollMessageBatch kc (Consumer.Timeout 500) (Consumer.BatchSize pollBatchSize)
      forM_ (Either.lefts polled) $ \err ->
        unless (any (`isRdKafkaError` err) quietPollErrors) $ flow $ logError $ "driver-performance: kafka poll error: " <> show err
      lp' <- handleRecords conn lp (Either.rights polled)
      now <- nowMs
      lp'' <- if isNothing lp'.lpReplay && now >= lp'.lpNextFlushMs then flushWindow conn now Nothing lp' else pure lp'
      pollLoop conn lp''

    -- While replaying, only records up to the recorded offsets are applied. The
    -- flush waits until every recorded partition reached its last offset, however
    -- long that takes: a partial window under the original token is dropped by
    -- ClickHouse when the first insert landed, and its unreached records would
    -- later be inserted again under a new token. A partition whose recorded
    -- offsets were deleted by retention resumes past them (auto.offset.reset),
    -- so its next record still reaches the end.
    -- Records of a partition not yet sought back came from the wrong offset:
    -- they are dropped, and delivered again after the seek.
    handleRecords :: R.Connection -> Loop -> [ConsumerRecordD] -> IO Loop
    handleRecords conn lp records = case lp.lpReplay of
      Nothing -> applyRecords conn lp records
      Just rp0 -> do
        let usable = filter (\cr -> M.notMember (recordPartition cr) rp0.replaySeeks) records
        rp <- seekAssigned rp0
        let (inside, outside) = DL.partition (withinReplay rp) usable
            rp' = rp {replayEnds = foldl' reachEnd rp.replayEnds usable, replayHeld = reverse outside <> rp.replayHeld}
        lp' <- applyRecords conn (lp {lpReplay = Just rp'}) inside
        now <- nowMs
        if
            | M.null rp'.replayEnds -> do
              flushed <- flushWindow conn now (Just rp.replayToken) (lp' {lpReplay = Nothing})
              applyRecords conn flushed (reverse rp'.replayHeld)
            | now >= rp'.replayAlertAtMs -> do
              flow $ logError $ "driver-performance: replay of the unfinished flush " <> rp.replayToken <> " has still not reached " <> show (M.toList rp'.replayEnds) <> " (not yet sought back: " <> show (M.toList rp'.replaySeeks) <> "); holding " <> show (length rp'.replayHeld) <> " later records, nothing is flushed or committed until it does"
              pure (lp' {lpReplay = Just (rp' {replayAlertAtMs = now + replayAlertEveryMs})})
            | otherwise -> pure lp'

    -- Seeks every partition still to be sought that is assigned by now, one at
    -- a time; the others are tried again on the next poll.
    seekAssigned :: Replay -> IO Replay
    seekAssigned rp
      | M.null rp.replaySeeks = pure rp
      | otherwise = do
        assignedRes <- Consumer.assignment kc
        case assignedRes of
          Left err -> do
            flow $ logWarning $ "driver-performance: cannot read the assignment, replay seek deferred: " <> show err
            pure rp
          Right assignedNow -> do
            let isAssigned (t, p) = maybe False (Consumer.PartitionId p `elem`) (M.lookup (Consumer.TopicName t) assignedNow)
            sought <- forM (filter (isAssigned . fst) (M.toList rp.replaySeeks)) $ \((t, p), firstOffset) -> do
              seekErr <- Consumer.seek kc (Consumer.Timeout 10000) [Consumer.TopicPartition (Consumer.TopicName t) (Consumer.PartitionId p) (Consumer.PartitionOffset firstOffset)]
              case seekErr of
                Just err -> do
                  flow $ logWarning $ "driver-performance: replay seek of " <> t <> "/" <> show p <> " to offset " <> show firstOffset <> " failed, retrying: " <> show err
                  pure Nothing
                Nothing -> do
                  flow $ logInfo $ "driver-performance: replay sought " <> t <> "/" <> show p <> " back to offset " <> show firstOffset
                  pure (Just (t, p))
            pure (rp {replaySeeks = foldl' (flip M.delete) rp.replaySeeks (catMaybes sought)})

    applyRecords :: R.Connection -> Loop -> [ConsumerRecordD] -> IO Loop
    applyRecords conn lp records
      | null records = pure lp
      | otherwise = do
        let outcomes = [(cr, decodeRecord cr) | cr <- records]
            msgs = [m | (_, Right (Just m)) <- outcomes]
        forM_ [(cr, e) | (cr, Left e) <- outcomes] $ \(cr, e) ->
          flow $ logWarning $ "driver-performance: skipped undecodable message on " <> recordTopic cr <> " at offset " <> show (recordOffset cr) <> ": " <> e
        -- One round trip for every key the batch may read, a second for keys
        -- found through it (the ride behind an index, a pending booking's fare params).
        loaded1 <- loadKeys conn (foldl' trackOffset lp.lpWindow records) (concatMap firstKeys msgs)
        loaded2 <- loadKeys conn loaded1 (concatMap (secondKeys loaded1) msgs)
        -- A third round: the driver's state at the event time of each entity
        -- seen for the first time (or a ride moving to another driver), once
        -- the writer has applied events up to the newest of those times.
        let lookups = stampLookups loaded2 msgs
            lookupTimes = map snd lookups
        waitNow <- nowMs
        -- Capped at now, so an event stamped ahead of the clock can't hold the batch.
        unless (null lookupTimes) $ waitForWriter conn (min waitNow (maximum lookupTimes))
        found <- retryingAfter loadRetryMicros "driver state history read" (statesAt conn lookups)
        -- An entry written before snapshots carried @state@ counts as no state.
        let stamps = M.mapMaybe (\(entryTs, snapshot) -> if AKM.null snapshot.state then Nothing else Just (entryTs, stateTag snapshot.state)) found
            withLookups = if null lookupTimes then loaded2 else loaded2 {wMinLookupMs = Just (minimum (maybeToList loaded2.wMinLookupMs <> lookupTimes))}
            applied = foldl' (applyMsg stamps) withLookups msgs
        forM_ (reverse applied.wNotes) (flow . logDebug)
        pure (lp {lpWindow = applied {wNotes = []}})

    -- Until state_writer:watermark reaches @needed@ (epoch ms), or
    -- 'writerWaitTimeoutMs' passed; an unreadable watermark counts as not reached.
    waitForWriter :: R.Connection -> Integer -> IO ()
    waitForWriter conn needed = go 0
      where
        go waitedMs = do
          reply <- (Right <$> R.runRedis conn (R.get (TE.encodeUtf8 writerWatermarkKey))) `catchAny` (pure . Left)
          let reached = case reply of
                Right (Right (Just wmBytes)) -> maybe False (>= needed) (A.decodeStrict wmBytes :: Maybe Integer)
                _ -> False
          if
              | reached -> pure ()
              | waitedMs >= writerWaitTimeoutMs -> flow $ logWarning $ "driver-performance: state writer has not reached " <> show needed <> " after " <> show writerWaitTimeoutMs <> " ms, looking states up anyway"
              | otherwise -> threadDelay writerPollMicros >> go (waitedMs + 200)

    -- Keys already read or written this window are not read again.
    loadKeys :: R.Connection -> Window -> [Text] -> IO Window
    loadKeys conn w wanted = case S.toList (S.fromList (filter (\k -> not (M.member k w.wDirty || M.member k w.wLoaded)) wanted)) of
      [] -> pure w
      missing -> do
        fetched <- retryingAfter loadRetryMicros "state read" $ do
          replies <- R.runRedis conn (forM (inChunks redisChunk missing) (R.mget . map TE.encodeUtf8))
          pure $ case sequence replies of
            Left reply -> Left (show reply)
            Right values -> Right (M.fromList (zip missing (map (>>= A.decodeStrict) (concat values))))
        pure (w {wLoaded = M.union fetched w.wLoaded})

    retryingAfter :: Int -> Text -> IO (Either Text a) -> IO a
    retryingAfter delayMicros what action = do
      res <- action `catchAny` (pure . Left . show)
      case res of
        Right a -> pure a
        Left err -> do
          flow $ logError $ "driver-performance: " <> what <> " failed, retrying: " <> err
          threadDelay delayMicros
          retryingAfter delayMicros what action

    flushWindow :: R.Connection -> Integer -> Maybe Text -> Loop -> IO Loop
    flushWindow conn now mbToken lp = do
      let w = lp.lpWindow
          token = fromMaybe (offsetsToken w.wOffsets) mbToken
          rows = filter (not . nullMeasures . snd) (M.toList w.wDeltas)
      unless (null rows) $
        retryingAfter flushRetryMicros "flush marker write" $
          writeMarker conn Inflight {inflightToken = token, inflightOffsets = [(t, p, firstOffset, lastOffset) | ((t, p), (firstOffset, lastOffset)) <- M.toList w.wOffsets]}
      retryingAfter flushRetryMicros "insert" (insertRows token (map renderRow rows))
      retryingAfter flushRetryMicros "state write" (writeState conn (M.toList w.wDirty))
      commitOffsets w.wOffsets
      finishFlush conn now ((\minLookupMs -> minLookupMs - horizonMarginMs) <$> w.wMinLookupMs)
      flow $ logInfo $ "driver-performance: flushed " <> show (length rows) <> " rows, " <> show (M.size w.wDirty) <> " state keys"
      pure (lp {lpWindow = emptyWindow, lpNextFlushMs = now + flushEveryMs})

    insertRows :: Text -> [Text] -> IO (Either Text ())
    insertRows _ [] = pure (Right ())
    insertRows token rowLines = do
      let query = "INSERT INTO driver_analytics_5m SETTINGS insert_deduplication_token = '" <> token <> "', date_time_input_format = 'best_effort' FORMAT JSONEachRow\n" <> T.intercalate "\n" rowLines
      res <- flow (runExecQuery' (ExprStr (T.unpack (asciiJson query))) ATLAS_KAFKA)
      pure (either (Left . T.pack) (const (Right ())) res)

    writeMarker :: R.Connection -> Inflight -> IO (Either Text ())
    writeMarker conn marker = replyResult <$> R.runRedis conn (R.setex (TE.encodeUtf8 inflightKey) rideTtl (LBS.toStrict (A.encode marker)))

    writeState :: R.Connection -> [(Text, (A.Value, Integer))] -> IO (Either Text ())
    writeState conn entries = do
      replies <- forM (inChunks redisChunk entries) $ \chunk ->
        R.runRedis conn (forM chunk (\(k, (v, ttl)) -> R.setex (TE.encodeUtf8 k) ttl (LBS.toStrict (A.encode v))))
      pure (replyResult (sequence (concat replies)))

    -- Exactly the applied records: records held back during a replay stay uncommitted.
    commitOffsets :: M.Map (Text, Int) (Int64, Int64) -> IO ()
    commitOffsets offs = unless (M.null offs) $ do
      commitErr <-
        Consumer.commitPartitionsOffsets Consumer.OffsetCommit kc $
          [Consumer.TopicPartition (Consumer.TopicName t) (Consumer.PartitionId p) (Consumer.PartitionOffset (lastOffset + 1)) | ((t, p), (_, lastOffset)) <- M.toList offs]
      whenJust commitErr $ \err -> flow $ logError $ "driver-performance: offset commit failed: " <> show err

    -- perf:state_horizon (1-hour expiry; only set when the window looked a
    -- state up, so a lagging consumer never moves it to the wall clock): the
    -- writer trims driver state history only
    -- before it, so the next window's lookups still find their state.
    finishFlush :: R.Connection -> Integer -> Maybe Integer -> IO ()
    finishFlush conn now mbHorizon = do
      let withHorizon prior = case mbHorizon of
            Just horizonMs -> prior >> R.setex (TE.encodeUtf8 stateHorizonKey) horizonTtl (TE.encodeUtf8 (show horizonMs))
            Nothing -> prior
      res <- (replyResult <$> R.runRedis conn (withHorizon (R.del [TE.encodeUtf8 inflightKey] >> R.set (TE.encodeUtf8 watermarkKey) (TE.encodeUtf8 (show now))))) `catchAny` (pure . Left . show)
      either (\err -> flow $ logError $ "driver-performance: watermark update failed: " <> err) pure res

    -- Replays the recorded partitions the group has not committed past. One it
    -- never committed starts wherever auto.offset.reset puts it, so it is
    -- sought back to its recorded first offset once assigned. Both reads are
    -- retried until they succeed: going on without the replay could insert the
    -- window again under a new token.
    loadReplay :: R.Connection -> IO (Maybe Replay)
    loadReplay conn = do
      markerRaw <- retryingAfter loadRetryMicros "flush marker read" (either (Left . show) Right <$> R.runRedis conn (R.get (TE.encodeUtf8 inflightKey)))
      case markerRaw of
        Nothing -> pure Nothing
        Just markerBytes -> case A.decodeStrict markerBytes :: Maybe Inflight of
          Nothing -> do
            flow $ logError $ "driver-performance: cannot decode " <> inflightKey <> ", not replaying it: " <> show markerBytes
            pure Nothing
          Just inflight -> do
            let subscribed = map Consumer.unTopicName appEnv.kafkaConsumerCfg.topicNames
                recorded = [entry | entry@(t, _, _, _) <- inflight.inflightOffsets, t `elem` subscribed]
            tps <-
              if null recorded
                then pure []
                else retryingAfter loadRetryMicros "committed offsets read" (either (Left . show) Right <$> Consumer.committed kc (Consumer.Timeout 10000) [(Consumer.TopicName t, Consumer.PartitionId p) | (t, p, _, _) <- recorded])
            let committedAt = M.fromList [((Consumer.unTopicName (Consumer.tpTopicName tp), Consumer.unPartitionId (Consumer.tpPartition tp)), o) | tp <- tps, Consumer.PartitionOffset o <- [Consumer.tpOffset tp]]
                unfinished = [entry | entry@(t, p, _, lastOffset) <- recorded, maybe True (<= lastOffset) (M.lookup (t, p) committedAt)]
                ends = M.fromList [((t, p), lastOffset) | (t, p, _, lastOffset) <- unfinished]
                seeks = M.fromList [((t, p), firstOffset) | (t, p, firstOffset, _) <- unfinished, M.notMember (t, p) committedAt]
            if M.null ends
              then pure Nothing
              else do
                flow $ logInfo $ "driver-performance: replaying the unfinished flush " <> inflight.inflightToken
                alertAt <- (+ replayAlertEveryMs) <$> nowMs
                pure (Just Replay {replayToken = inflight.inflightToken, replayEnds = ends, replaySeeks = seeks, replayHeld = [], replayAlertAtMs = alertAt})

withinReplay :: Replay -> ConsumerRecordD -> Bool
withinReplay rp cr = maybe False (recordOffset cr <=) (M.lookup (recordPartition cr) rp.replayEnds)

reachEnd :: M.Map (Text, Int) Int64 -> ConsumerRecordD -> M.Map (Text, Int) Int64
reachEnd ends cr = case M.lookup (recordPartition cr) ends of
  Just lastOffset | recordOffset cr >= lastOffset -> M.delete (recordPartition cr) ends
  _ -> ends

------------------------------------------------------------
-- Applying one message (pure)
------------------------------------------------------------

-- | (driver id, epoch ms) -> the stamp of the driver's state at that time,
-- for the lookups 'stampLookups' asked for; a lookup that found no state is absent.
type Stamps = M.Map (Text, Integer) Stamp

applyMsg :: Stamps -> Window -> CdcMsg -> Window
applyMsg stamps w = \case
  MsgSrd r -> applySrd stamps r w
  MsgRide r -> applyRide stamps r w
  MsgDriverBooking r -> applyDriverBooking r w
  MsgCancellation r -> applyCancellation r w
  MsgFareParams r -> applyFareParams r w
  MsgRating r -> applyRating r w

applySrd :: Stamps -> SrdRow -> Window -> Window
applySrd stamps r w
  | r.srdDriverId == sentinelDriverId = w
  | otherwise = case storedSrd r.srdId w of
    Just prev | srdRank r < prev.srdStateRank -> w
    prevState ->
      let stamp = maybe (stampAt stamps r.srdDriverId r.srdCreatedAt) (.srdStateStamp) prevState
          new = srdContribution stamp r
       in putStored srdTtl (srdKey r.srdId) SrdState {srdStateRank = srdRank r, srdStateStamp = stamp, srdStateContribution = new} (emit (prevState >>= (.srdStateContribution)) new w)

-- | A lower-rank version never replaces a higher one; an equal rank replaces
-- (fare updates on COMPLETED).
applyRide :: Stamps -> RideRow -> Window -> Window
applyRide stamps r w = case storedRide r.rideId w of
  Just prev
    | r.rideRank < prev.rsRow.rideRank -> withNote ("ride " <> r.rideId <> ": " <> r.rideStatus <> " after " <> prev.rsRow.rideStatus <> " ignored") w
  prevState ->
    let facts = maybe (pendingFacts r w) (.rsFacts) prevState
        stamp = case prevState of
          Just prev | prev.rsRow.rideDriverId == r.rideDriverId -> prev.rsStamp
          _ -> stampAt stamps r.rideDriverId r.rideCreatedAt
     in saveRide (prevState >>= (.rsContribution)) RideState {rsRow = r, rsStamp = stamp, rsFacts = facts, rsContribution = Nothing} w

applyDriverBooking :: DriverBookingRow -> Window -> Window
applyDriverBooking r w = case rideByBooking r.dbkId w of
  Just st -> updateFacts (\facts -> withPendingFareParams (Just r.dbkFareParamsId) w (facts {factDriverBooking = Just fact})) st w
  Nothing -> putStored pendingTtl (pendingDriverBookingKey r.dbkId) fact w
  where
    fact = DriverBookingFact {dbfAutoAccepted = r.dbkAutoAccepted, dbfFareParamsId = r.dbkFareParamsId}

-- | Joined by ride_id, or by booking_id when ride_id is null.
applyCancellation :: CancellationRow -> Window -> Window
applyCancellation r w = case knownRide of
  Just st -> updateFacts (\facts -> facts {factCancelSource = Just r.bcrSource}) st w
  Nothing -> putStored pendingTtl pendingKey r.bcrSource w
  where
    (knownRide, pendingKey) = case r.bcrRideId of
      Just rid -> (storedRide rid w, pendingCancelByRideKey rid)
      Nothing -> (rideByBooking r.bcrBookingId w, pendingCancelByBookingKey r.bcrBookingId)

-- | Kept pending only with a non-zero value (about 1.6% of rows) or to replace
-- a pending one, but always applied to a ride already known through the index.
-- A pending one is replaced then too: later booking and ride messages attach
-- the pending version to the ride again.
applyFareParams :: FareParamsRow -> Window -> Window
applyFareParams r w = case storedText (fareParamsRideKey r.fpId) w >>= (`storedRide` w) of
  Just st -> updateFacts (\facts -> facts {factFareParams = if carriesValue then M.insert r.fpId fact facts.factFareParams else M.delete r.fpId facts.factFareParams}) st (if hasPending then putPending w else w)
  Nothing
    | carriesValue || hasPending -> putPending w
    | otherwise -> w
  where
    fact = FareParamsFact {fpfDriverSelectedFare = r.fpDriverSelectedFare, fpfCustomerExtraFee = r.fpCustomerExtraFee}
    carriesValue = any (maybe False (/= 0)) [r.fpDriverSelectedFare, r.fpCustomerExtraFee]
    hasPending = isJust (storedFareParams r.fpId w)
    putPending = putStored fareParamsTtl (pendingFareParamsKey r.fpId) fact

applyRating :: RatingRow -> Window -> Window
applyRating r w = case storedRide r.ratingRideId w of
  Just st -> updateFacts (\facts -> facts {factRatings = M.insert r.ratingId r.ratingValue facts.factRatings}) st w
  Nothing ->
    withNote ("rating " <> r.ratingId <> ": ride " <> r.ratingRideId <> " not known, kept pending") $
      putStored longPendingTtl (pendingRatingsKey r.ratingRideId) (M.insert r.ratingId r.ratingValue (storedRatings r.ratingRideId w)) w

-- | Every fact that arrived before the ride, under its own join key.
pendingFacts :: RideRow -> Window -> RideFacts
pendingFacts r w =
  withPendingFareParams ((.dbfFareParamsId) <$> driverBooking) w $
    RideFacts
      { factDriverBooking = driverBooking,
        factCancelSource = stored (pendingCancelByRideKey r.rideId) w <|> stored (pendingCancelByBookingKey bk) w,
        factFareParams = M.empty,
        factRatings = storedRatings r.rideId w
      }
  where
    bk = r.rideBookingId
    driverBooking = storedDriverBooking bk w

withPendingFareParams :: Maybe Text -> Window -> RideFacts -> RideFacts
withPendingFareParams mbFareParamsId w facts = fromMaybe facts $ do
  fid <- mbFareParamsId
  fact <- storedFareParams fid w
  pure (facts {factFareParams = M.insert fid fact facts.factFareParams})

-- | A fact for a known ride.
updateFacts :: (RideFacts -> RideFacts) -> RideState -> Window -> Window
updateFacts f st = saveRide st.rsContribution (st {rsFacts = f st.rsFacts})

-- | Recompute the ride's contribution, emit the change, store the ride and its
-- join indexes (rewritten with it, so they live as long as it does).
saveRide :: Contribution -> RideState -> Window -> Window
saveRide old st w = putStored rideTtl (rideKey rid) (st {rsContribution = new}) (emit old new (foldl' addIndex w joinKeys))
  where
    new = rideContribution st
    rid = st.rsRow.rideId
    joinKeys = bookingRideKey st.rsRow.rideBookingId : map fareParamsRideKey (maybeToList ((.dbfFareParamsId) <$> st.rsFacts.factDriverBooking))
    addIndex cur ik = putStored rideTtl ik rid cur

------------------------------------------------------------
-- State store view: this window's writes, then its reads
------------------------------------------------------------

stored :: FromJSON a => Text -> Window -> Maybe a
stored k w = fromStoredValue =<< maybe (join (M.lookup k w.wLoaded)) (Just . fst) (M.lookup k w.wDirty)

fromStoredValue :: FromJSON a => A.Value -> Maybe a
fromStoredValue v = case A.fromJSON v of
  A.Success a -> Just a
  A.Error _ -> Nothing

storedText :: Text -> Window -> Maybe Text
storedText = stored

storedSrd :: Text -> Window -> Maybe SrdState
storedSrd sid = stored (srdKey sid)

storedRide :: Text -> Window -> Maybe RideState
storedRide rid = stored (rideKey rid)

storedDriverBooking :: Text -> Window -> Maybe DriverBookingFact
storedDriverBooking bk = stored (pendingDriverBookingKey bk)

storedFareParams :: Text -> Window -> Maybe FareParamsFact
storedFareParams fid = stored (pendingFareParamsKey fid)

storedRatings :: Text -> Window -> M.Map Text Integer
storedRatings rid = fromMaybe M.empty . stored (pendingRatingsKey rid)

rideByBooking :: Text -> Window -> Maybe RideState
rideByBooking bk w = storedText (bookingRideKey bk) w >>= (`storedRide` w)

putStored :: ToJSON a => Integer -> Text -> a -> Window -> Window
putStored ttl k v w = w {wDirty = M.insert k (A.toJSON v, ttl) w.wDirty}

emit :: Contribution -> Contribution -> Window -> Window
emit old new w = w {wDeltas = foldl' (\deltas (rk, ms) -> M.insertWith addMeasures rk ms deltas) w.wDeltas (diffContributions old new)}

withNote :: Text -> Window -> Window
withNote line w = w {wNotes = line : w.wNotes}

-- | Every key a message may read: its own state, and each fact or index it joins on.
firstKeys :: CdcMsg -> [Text]
firstKeys = \case
  MsgSrd r -> [srdKey r.srdId]
  MsgRide r ->
    let bk = r.rideBookingId
     in [rideKey r.rideId, pendingDriverBookingKey bk, pendingCancelByRideKey r.rideId, pendingCancelByBookingKey bk, pendingRatingsKey r.rideId]
  MsgDriverBooking r -> [bookingRideKey r.dbkId, pendingFareParamsKey r.dbkFareParamsId]
  MsgCancellation r -> maybe [bookingRideKey r.bcrBookingId] (pure . rideKey) r.bcrRideId
  MsgFareParams r -> [fareParamsRideKey r.fpId, pendingFareParamsKey r.fpId]
  MsgRating r -> [rideKey r.ratingRideId, pendingRatingsKey r.ratingRideId]

-- | The (driver id, epoch ms) states a batch needs: the event time of every
-- pop-up and ride with no state yet, and of every ride version whose driver
-- differs from its stored one or from another version in the batch.
stampLookups :: Window -> [CdcMsg] -> [(Text, Integer)]
stampLookups w msgs = concatMap wanted msgs
  where
    batchDrivers = M.fromListWith S.union [(r.rideId, S.singleton r.rideDriverId) | MsgRide r <- msgs]
    movesInBatch rid = maybe False ((> 1) . S.size) (M.lookup rid batchDrivers)
    wanted = \case
      MsgSrd r | isNothing (storedSrd r.srdId w) -> [(r.srdDriverId, r.srdCreatedAt * 1000)]
      MsgRide r
        | maybe True (\prev -> prev.rsRow.rideDriverId /= r.rideDriverId) (storedRide r.rideId w) || movesInBatch r.rideId ->
          [(r.rideDriverId, r.rideCreatedAt * 1000)]
      _ -> []

-- | The stamp looked up for a driver at an event time in epoch seconds.
stampAt :: Stamps -> Text -> Integer -> Maybe Stamp
stampAt stamps driver eventSecs = M.lookup (driver, eventSecs * 1000) stamps

-- | Keys found through the first round: the ride behind an index, and the
-- quote fare parameters of a pending driver booking.
secondKeys :: Window -> CdcMsg -> [Text]
secondKeys w = \case
  MsgRide r -> [pendingFareParamsKey dbk.dbfFareParamsId | Just dbk <- [storedDriverBooking r.rideBookingId w]]
  MsgDriverBooking r -> viaIndex (bookingRideKey r.dbkId)
  MsgCancellation r -> if isJust r.bcrRideId then [] else viaIndex (bookingRideKey r.bcrBookingId)
  MsgFareParams r -> viaIndex (fareParamsRideKey r.fpId)
  _ -> []
  where
    viaIndex ik = map rideKey (maybeToList (storedText ik w))

------------------------------------------------------------
-- Kafka records, rows, helpers
------------------------------------------------------------

-- | Dispatched on the full topic name; the key (a stream name or shard key) is never used.
decodeRecord :: ConsumerRecordD -> Either Text (Maybe CdcMsg)
decodeRecord cr = maybe (Right Nothing) (decodeCdc (recordTopic cr)) (Consumer.crValue cr)

recordTopic :: ConsumerRecordD -> Text
recordTopic = Consumer.unTopicName . Consumer.crTopic

recordPartition :: ConsumerRecordD -> (Text, Int)
recordPartition cr = (recordTopic cr, Consumer.unPartitionId (Consumer.crPartition cr))

recordOffset :: ConsumerRecordD -> Int64
recordOffset = Consumer.unOffset . Consumer.crOffset

trackOffset :: Window -> ConsumerRecordD -> Window
trackOffset w cr = w {wOffsets = M.insertWith (\(_, newest) (oldest, _) -> (oldest, newest)) (recordPartition cr) (o, o) w.wOffsets}
  where
    o = recordOffset cr

-- | Same offsets -> same token, so ClickHouse drops a re-sent identical insert.
offsetsToken :: M.Map (Text, Int) (Int64, Int64) -> Text
offsetsToken m =
  "driver-performance:" <> T.intercalate "," [t <> "/" <> show p <> "/" <> show a <> "-" <> show b | ((t, p), (a, b)) <- M.toList m]

-- | One JSONEachRow line; omitted columns default to 0. Money is written as a
-- plain decimal, never in exponent form. The state columns are the stamp's
-- state; without one they are omitted (NULL / []) and state_known is 0.
renderRow :: (StampedKey, Measures) -> Text
renderRow (((city, bucketStart, driver), stamp), ms) = "{" <> T.intercalate "," (keyFields <> columnFields <> stateFields) <> "}"
  where
    stateFields = case stamp of
      Nothing -> ["\"state_known\":0"]
      Just (entryTs, tagText) ->
        ["\"state_known\":1", "\"state_at\":" <> show entryTs]
          <> [jsonText (AK.toText columnKey) <> ":" <> TE.decodeUtf8 (LBS.toStrict (A.encode value)) | (columnKey, value) <- renderStateValues (stateOfTag tagText)]
    keyFields =
      [ "\"bucket\":" <> show bucketStart,
        "\"merchant_operating_city_id\":" <> jsonText city,
        "\"driver_id\":" <> jsonText driver
      ]
    columnFields = ["\"" <> column <> "\":" <> (if column `elem` moneyColumns then fixed4 value else show value) | (column, value) <- M.toList ms.msColumns]

jsonText :: Text -> Text
jsonText = TE.decodeUtf8 . LBS.toStrict . A.encode

-- | Ten-thousandths as a decimal: 1510840 -> 151.0840.
fixed4 :: Integer -> Text
fixed4 n = (if n < 0 then "-" else "") <> show whole <> "." <> T.justifyRight 4 '0' (show frac)
  where
    (whole, frac) = abs n `quotRem` 10000

replyResult :: Show e => Either e a -> Either Text ()
replyResult = either (Left . show) (const (Right ()))

inChunks :: Int -> [a] -> [[a]]
inChunks chunkSize xs = case splitAt chunkSize xs of
  (chunk, []) -> [chunk | not (null chunk)]
  (chunk, rest) -> chunk : inChunks chunkSize rest

nowMs :: IO Integer
nowMs = round . (* 1000) <$> getPOSIXTime
