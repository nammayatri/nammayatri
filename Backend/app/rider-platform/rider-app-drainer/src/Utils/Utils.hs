module Utils.Utils where

import Config.Env
import Constants as C
import qualified Control.Concurrent as Control
import DBQuery.Functions (textToSnakeCaseText)
import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Text.Encoding as DTE
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as DTE
import qualified Data.UUID as UUID (toASCIIBytes)
import Data.UUID.V4 (nextRandom)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import GHC.Float (int2Double)
import Kafka.Producer
import qualified Kafka.Producer as KafkaProd
import qualified Kernel.Beam.Types as KBT
import Kernel.Streaming.Kafka.Producer (produceToSecondaryProducer)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Common
import System.Posix.Signals (raiseSignal, sigKILL)
import System.Random.PCG
import Types.DBSync
import Types.Event
import qualified Utils.Redis as RQ

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
    foldr
      ( \x r k -> case k of
          0 -> Just x
          _ -> r (k -1)
      )
      (const Nothing)
      xs
      n

executeInSequence :: (L.MonadFlow f, Show a1) => (t -> Text -> f (Either a1 a2)) -> ([a2], [a1]) -> Text -> [t] -> f ([a2], [a1])
executeInSequence _ store _ [] = pure store
executeInSequence func store dbStreamKey (command : commands) = do
  result <- func command dbStreamKey
  case result of
    Left id -> do
      let store' = second (id :) store
      L.logErrorT "EXECUTION_FAILURE" (show id) $> store'
    Right id' -> do
      let store' = first (id' :) store
      executeInSequence func store' dbStreamKey commands

stopDrainer :: Flow ()
stopDrainer = RQ.setValueInRedis C.drainerStopKey True >> publishDBSyncMetric (DrainerStopStatus 1)

randomInteger :: Integer -> Integer -> IO Integer
randomInteger a b = toInteger <$> randomInt (fromInteger a) (fromInteger b)

randomInt :: Int -> Int -> IO Int
randomInt a b = withSystemRandom $ uniformR (min a b, max a b)

delay :: Int -> IO ()
delay = threadDelay

-- Performs a simple sliding window rate limiting of 'n' operations for every
-- 'window' ms. Takes a history of operation times as state along with the
-- time of a just-performed operation. Returns an updated history that can
-- be used for the next iteration, along with how much time must elapse before
-- the next operation is performed.
tryRateLimiter :: Int -> Int -> History -> Int -> Int -> (History, Int)
tryRateLimiter n window history now count =
  let newVals = replicate count now
      trimmed = dropWhile (\lhs -> lhs < now - window) history
      history' = foldr (:) trimmed newVals
      earliest =
        if length history' <= n
          then 0
          else length history' - n
      waitTill = maybe now (window +) ((!?) history' earliest)
      waitMs = waitTill - now
   in (history', if length history' < n then 0 else waitMs)

publishDBSyncMetric :: DBSyncMetric -> Flow ()
publishDBSyncMetric metric = do
  environment <- ask
  L.runIO $ pubDBSyncMetric (_counterHandles environment) metric

publishProcessLatency :: Text -> Double -> Flow ()
publishProcessLatency processName latency = do
  L.logInfo (("LATENCY: " :: Text) <> processName) (show latency)
  void $ publishDBSyncMetric $ ProcessLatency processName latency

publishDrainLatency :: Text -> L.KVDBStreamEntryID -> Flow ()
publishDrainLatency action (L.KVDBStreamEntryID id _) = do
  time <- L.getCurrentDateInMillis
  let latency = int2Double time - int2Double (fromIntegral id)
  L.logInfo (("LATENCY: " :: Text) <> action) (show latency)
  void $ publishDBSyncMetric $ QueryDrainLatency action latency
  void $ publishDBSyncMetric $ DrainerLagSeconds (latency / 1000)

decodeToText :: ByteString -> Text
decodeToText = DTE.decodeUtf8With DTE.lenientDecode

decodeFromText :: FromJSON a => Text -> Maybe a
decodeFromText = A.decode . BSL.fromStrict . DTE.encodeUtf8

filterCreateCommands :: (L.KVDBStreamEntryID, DBCommand, ByteString) -> Maybe (L.KVDBStreamEntryID, ByteString)
filterCreateCommands (id, Create {}, val) = Just (id, val)
filterCreateCommands _ = Nothing

filterUpdateCommands :: (L.KVDBStreamEntryID, DBCommand, ByteString) -> Maybe (L.KVDBStreamEntryID, ByteString)
filterUpdateCommands (id, Update {}, val) = Just (id, val)
filterUpdateCommands _ = Nothing

filterDeleteCommands :: (L.KVDBStreamEntryID, DBCommand, ByteString) -> Maybe (L.KVDBStreamEntryID, ByteString)
filterDeleteCommands (id, Delete {}, val) = Just (id, val)
filterDeleteCommands _ = Nothing

getStreamName :: Text -> Flow (Maybe Text)
getStreamName streamName = do
  countWithErr <- RQ.incrementCounter $ T.pack C.ecRedisDBStreamCounter
  numberOfStreamsForKV' <- getNumOfStreams
  count <- case countWithErr of
    Right count -> pure (count `mod` numberOfStreamsForKV')
    Left _ -> L.runIO $ randomInteger 0 numberOfStreamsForKV'
  let dbStreamKey = streamName <> "{shard-" <> show count <> "}"
  let lockKeyName = dbStreamKey <> "_lock"
  resp <- RQ.setValueWithOptions lockKeyName "LOCKED" (L.Milliseconds 120000) L.SetIfNotExist
  case resp of
    Right True -> pure $ Just dbStreamKey
    Right _ -> pure Nothing
    Left _ -> pure Nothing

getNumOfStreams :: Flow Integer
getNumOfStreams = do
  tables <- L.getOption KBT.Tables >>= maybe (L.logError ("TABLES_NOT_FOUND" :: Text) "KV tables not found while getting number of streams" >> stopDrainer >> pure defaultTableData) pure
  let defaultShardMod = fromIntegral tables.defaultShardMod
      maxValueFromRange =
        fromMaybe C.numberOfStreamsForKV $
          getMaxValue (tables.tableShardModRange) -- Apply getMaxValue to shardRanges
  pure $ max defaultShardMod maxValueFromRange

getMaxValue :: HM.HashMap Text (Int, Int) -> Maybe Integer
getMaxValue hashmap =
  NE.nonEmpty (map snd $ HM.elems hashmap) >>= pure . fromIntegral . maximum

genSessionId :: IO C8.ByteString
genSessionId = do
  rawUUID <- nextRandom
  let sessId = C8.pack "DRAINSESSION-" <> C8.filter (/= '-') (UUID.toASCIIBytes rawUUID)
  pure sessId

-- Graceful shutdown utils

onSigINT :: MVar () -> IO ()
onSigINT stop = do
  putStrLn ("RECEIVED SIGINT" :: String)
  Control.putMVar stop ()

onSigTERM :: MVar () -> IO ()
onSigTERM stop = do
  putStrLn ("RECEIVED SIGTERM" :: String)
  Control.putMVar stop ()

shutDownHandler :: IO ()
shutDownHandler = do
  shutDownPeriod <- gracefulShutDownPeriodInMs
  putStrLn ("SHUTTING DOWN DRAINER in " ++ show ((fromIntegral shutDownPeriod :: Double) / 1000000) ++ " seconds" :: String)
  delay shutDownPeriod
  raiseSignal sigKILL

createInKafka :: KafkaProducerTools -> A.Value -> Text -> DBModel -> IO (Either Text ())
createInKafka kafkaProducerTools dbObject dbStreamKey model = do
  let topicName = "aap-sessionizer-" <> T.toLower model.getDBModel
      msg = message topicName dbObject
  -- Push to primary producer
  result' <- KafkaProd.produceMessage kafkaProducerTools.producer msg
  case result' of
    Just err -> pure $ Left $ T.pack ("Kafka Error: " <> show err)
    _ -> do
      -- Push to secondary producer if configured
      produceToSecondaryProducer kafkaProducerTools.secondaryProducer msg (\_ -> pure ())
      pure $ Right ()
  where
    message topicName event =
      ProducerRecord
        { prTopic = TopicName topicName,
          prPartition = UnassignedPartition,
          prKey = Just $ TE.encodeUtf8 dbStreamKey,
          prValue = Just . LBS.toStrict $ A.encode event
        }

shouldPushToKafkaOnly :: DBModel -> [Text] -> Bool
shouldPushToKafkaOnly model _dontEnableDbTables = textToSnakeCaseText model.getDBModel `elem` _dontEnableDbTables || model.getDBModel `elem` _dontEnableDbTables

shouldPushToDbOnly :: DBModel -> [Text] -> Bool
shouldPushToDbOnly model _dontEnableForKafka = textToSnakeCaseText model.getDBModel `elem` _dontEnableForKafka || model.getDBModel `elem` _dontEnableForKafka

-- Retry failed items with exponential backoff before sending to DLQ
retryFailedWithBackoff ::
  ((L.KVDBStreamEntryID, ByteString) -> Text -> Flow (Either L.KVDBStreamEntryID L.KVDBStreamEntryID)) ->
  Text ->
  [(L.KVDBStreamEntryID, ByteString)] ->
  [L.KVDBStreamEntryID] ->
  Int ->
  Int ->
  Flow ([L.KVDBStreamEntryID], [L.KVDBStreamEntryID])
retryFailedWithBackoff _ _ _ failedIds maxRetries attempt
  | attempt > maxRetries || null failedIds = pure ([], failedIds)
retryFailedWithBackoff runFn dbStreamKey allEntries failedIds maxRetries attempt = do
  let failedEntries = filter (\(entryId, _) -> entryId `elem` failedIds) allEntries
      backoffUs = min 10000000 (1000000 * (2 ^ (attempt - 1))) -- 1s, 2s, 4s... max 10s
  L.logWarning ("BATCH_RETRY" :: Text) $
    "Retrying " <> show (length failedEntries) <> " failed items, attempt " <> show attempt <> "/" <> show maxRetries
  void $ publishDBSyncMetric $ BatchRetryAttempt "Retry" attempt
  L.runIO $ delay backoffUs
  (retrySucc, retryFail) <- executeInSequence runFn ([], []) dbStreamKey failedEntries
  if null retryFail
    then pure (retrySucc, [])
    else do
      (moreSucc, stillFailed) <- retryFailedWithBackoff runFn dbStreamKey allEntries retryFail maxRetries (attempt + 1)
      pure (retrySucc ++ moreSucc, stillFailed)

-- Add failed items to the dead letter queue after exhausting retries
addToDLQ :: Text -> [(L.KVDBStreamEntryID, ByteString)] -> [L.KVDBStreamEntryID] -> Text -> Flow ()
addToDLQ dlqStreamKey allEntries failedIds action = do
  let failedEntries = filter (\(entryId, _) -> entryId `elem` failedIds) allEntries
  void $ RQ.addValueToErrorQueue dlqStreamKey ((\(_, bts) -> ("command", bts)) <$> failedEntries)
  void $ publishDBSyncMetric $ DLQItemAdded action (length failedEntries)
  L.logError ("DLQ_ITEMS_ADDED" :: Text) $
    "Added " <> show (length failedEntries) <> " " <> action <> " items to DLQ after max retries"

-- Check pool utilization and apply backpressure delay if threshold exceeded
checkBackpressure :: Flow ()
checkBackpressure = do
  Env {..} <- ask
  utilization <- L.runIO $ readIORef _poolUtilization
  threshold <- L.runIO getBackpressureThreshold
  when (utilization > threshold) $ do
    backpressureDelayUs <- L.runIO getBackpressureDelay
    L.logWarning ("BACKPRESSURE" :: Text) $
      "Pool utilization " <> show utilization <> " exceeds threshold " <> show threshold <> ", delaying " <> show (backpressureDelayUs `div` 1000) <> "ms"
    void $ publishDBSyncMetric $ BackpressureActivated utilization
    L.runIO $ delay backpressureDelayUs
