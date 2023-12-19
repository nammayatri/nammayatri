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
import qualified Kafka.Producer as Producer
import System.Posix.Signals (raiseSignal, sigKILL)
import System.Random.PCG
import Types.DBSync
import Types.Event
import qualified Utils.Redis as RQ

-- uncomment for debug purposes
-- import qualified Data.ByteString as BS
-- import qualified Data.Time.Clock.POSIX as Time
-- import qualified EulerHS.Language as EL
-- import Kernel.Prelude (UTCTime)
-- import qualified System.Directory as Dir
-- import System.FilePath.Posix as Path

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

publishDrainLatency :: Text -> L.KVDBStreamEntryID -> Flow ()
publishDrainLatency action (L.KVDBStreamEntryID id _) = do
  time <- L.getCurrentDateInMillis
  let latency = int2Double time - int2Double (fromIntegral id)
  L.logInfo (("LATENCY: " :: Text) <> action) (show latency)
  void $ publishDBSyncMetric $ QueryDrainLatency action latency

publishProcessLatency :: Text -> Double -> Flow ()
publishProcessLatency processName latency = do
  L.logInfo (("LATENCY: " :: Text) <> processName) (show latency)
  void $ publishDBSyncMetric $ ProcessLatency processName latency

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
  count <- case countWithErr of
    Right count -> pure (count `mod` C.numberOfStreamsForKV)
    Left _ -> L.runIO $ randomInteger 0 C.numberOfStreamsForKV
  let dbStreamKey = streamName <> "{shard-" <> show count <> "}"
  let lockKeyName = dbStreamKey <> "_lock"
  resp <- RQ.setValueWithOptions lockKeyName "LOCKED" (L.Milliseconds 120000) L.SetIfNotExist
  case resp of
    Right True -> pure $ Just dbStreamKey
    Right _ -> pure Nothing
    Left _ -> pure Nothing

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

createInKafka :: Producer.KafkaProducer -> A.Value -> Text -> DBModel -> IO (Either Text ())
createInKafka producer dbObject dbStreamKey model = do
  let topicName = "adob-sessionizer-" <> T.toLower model.getDBModel
  result' <- KafkaProd.produceMessage producer (message topicName dbObject)
  case result' of
    Just err -> pure $ Left $ T.pack ("Kafka Error: " <> show err)
    _ -> pure $ Right ()
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

-- FIXME KVDBStreamEntryID can be the same for different entries
-- uncomment for debug purposes
-- writeDebugFile ::
--   String ->
--   DBModel ->
--   EL.KVDBStreamEntryID ->
--   String ->
--   BS.ByteString ->
--   ReaderT Env EL.Flow ()
-- writeDebugFile action dbModel entryId fileName entity = EL.runIO $ do
--   let fullPath'Name = "/tmp/drainer/driver/" <> action <> "/" <> T.unpack dbModel.getDBModel <> "/" <> show (mkTimeStamp entryId) <> "/" <> fileName
--   let fullPath = Path.takeDirectory fullPath'Name
--   Dir.createDirectoryIfMissing True fullPath
--   BS.writeFile fullPath'Name entity

-- mkTimeStamp :: EL.KVDBStreamEntryID -> UTCTime
-- mkTimeStamp (EL.KVDBStreamEntryID posixTime _) = Time.posixSecondsToUTCTime $ fromInteger (posixTime `div` 1000)
