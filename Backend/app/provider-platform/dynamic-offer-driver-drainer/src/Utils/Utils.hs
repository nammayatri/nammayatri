{-# OPTIONS_GHC -Wno-type-defaults #-}

module Utils.Utils where

import Config.Env
import Constants as C
import qualified Control.Concurrent as Control
import Control.Exception (throwIO)
import Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import Data.Text.Encoding as DTE
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as DTE
import qualified Data.UUID as UUID (toASCIIBytes)
import Data.UUID.V4 (nextRandom)
import qualified Data.Vector as V
import Database.Beam.Postgres
import Database.PostgreSQL.Simple hiding (QueryError)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import GHC.Float (int2Double)
import Kafka.Producer
import qualified Kafka.Producer as KafkaProd
import qualified Kafka.Producer as Producer
import System.Posix.Signals (raiseSignal, sigKILL)
import System.Random.PCG
import Text.Casing (camel, quietSnake)
import Types.DBSync
import Types.Event
import qualified Utils.Redis as RQ
import Prelude (head)

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

-- functions being used in new drainer code ----------

newtype QueryError = QueryError Text
  deriving (Show)

instance Exception QueryError

-- Execute a query and throw a custom error if it fails

lookupText :: Text -> A.Object -> Maybe Text
lookupText key obj = do
  A.String txt <- HM.lookup key obj
  return txt

lookupArray :: Text -> A.Object -> Maybe A.Array
lookupArray key obj = do
  A.Array arr <- HM.lookup key obj
  return arr

lookupObject :: Text -> A.Object -> Maybe A.Object
lookupObject key obj = do
  A.Object obj' <- HM.lookup key obj
  return obj'

executeQuery :: Connection -> Query -> IO ()
executeQuery conn query' = do
  result <- try $ execute_ conn query' :: IO (Either SomeException Int64)
  case result of
    Left e -> throwIO $ QueryError $ "Query execution failed: " <> T.pack (show e)
    Right _ -> return ()

-- this will convert the model tag to model name
getModelname :: Text -> Int -> Text
getModelname modelTag num = textToSnakeCaseText (T.take (T.length modelTag - num) modelTag)

quote' :: Text -> Text
quote' t = "\"" <> t <> "\""

quote :: Text -> Text
quote t = "'" <> t <> "'"

textToSnakeCaseText :: Text -> Text
textToSnakeCaseText = T.pack . quietSnake . T.unpack

-- | we are setting mappings in case of beamColumn name is different from the Db column name
replaceMappings :: Text -> A.Object -> Text
replaceMappings element obj =
  case HM.lookup element obj of
    Just (A.String value) -> value
    _ -> element

extractValues :: Value -> [Value]
extractValues (Array arr) = toList arr
extractValues _ = []

extractBothValues :: Value -> Maybe (Text, Value)
extractBothValues (Object obj) = do
  mbVal <- (,) <$> HM.lookup "value0" obj <*> HM.lookup "value1" obj
  case mbVal of
    (A.String val0, val1) -> Just (val0, val1)
    _ -> Nothing
extractBothValues _ = Nothing

valueToText :: A.Value -> T.Text
valueToText (A.String t) = quote t
valueToText (A.Number n) =
  quote $
    if Sci.isInteger n
      then T.pack (show (Sci.coefficient n)) -- Convert to integer if it's an integer
      else T.pack (show (Sci.toRealFloat n)) -- Convert to floating-point
valueToText (A.Bool b) = quote $ if b then "true" else "false"
valueToText (A.Array a) = quote $ "{" <> T.intercalate "," (map valueToText' (V.toList a)) <> "}" --in case of array of value of a key in object
valueToText (A.Object obj) = quote $ T.pack (show (A.encode obj))
valueToText A.Null = "null"

valueToText' :: A.Value -> T.Text
valueToText' (A.String t) = t
valueToText' (A.Number n) =
  if Sci.isInteger n
    then T.pack (show (Sci.coefficient n))
    else T.pack (show (Sci.toRealFloat n))
valueToText' (A.Bool b) = if b then "true" else "false"
valueToText' (A.Array a) = "[" <> T.intercalate "," (map valueToText' (V.toList a)) <> "]"
valueToText' (A.Object obj) = T.pack (show (A.encode obj))
valueToText' _ = "null"

valueToTextForInConditions :: A.Value -> T.Text
valueToTextForInConditions (A.String t) = quote t
valueToTextForInConditions (A.Array ar) = "(" <> T.intercalate "," (map valueToTextForInConditions (V.toList ar)) <> ")"
valueToTextForInConditions _ = "null"

-- this is being used to get the where condition from the json object | Note : INCOMPLETE IMPLEMENTATION
makeWhereCondition :: Value -> A.Object -> Text
makeWhereCondition values mappings = case values of
  A.Object obj -> do
    let key = HM.keys obj
    case key of
      ["$and"] -> getArrayConditionText (fromMaybe A.emptyObject (HM.lookup "$and" obj)) " AND " mappings
      ["$or"] -> getArrayConditionText (fromMaybe A.emptyObject (HM.lookup "$or" obj)) " OR " mappings
      -- these conditions need to be implemented safely before using in production
      ["$gt"] -> quote' (textToSnakeCaseText $ replaceMappings (head key) mappings) <> " > " <> valueToText (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$gte"] -> quote' (textToSnakeCaseText $ replaceMappings (head key) mappings) <> " >= " <> valueToText (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$lt"] -> quote' (textToSnakeCaseText $ replaceMappings (head key) mappings) <> " < " <> valueToText (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$lte"] -> quote' (textToSnakeCaseText $ replaceMappings (head key) mappings) <> " <= " <> valueToText (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$ne"] -> quote' (textToSnakeCaseText $ replaceMappings (head key) mappings) <> " != " <> valueToText (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$notIn"] -> quote' (textToSnakeCaseText $ replaceMappings (head key) mappings) <> " NOT IN " <> valueToTextForInConditions (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$in"] -> quote' (textToSnakeCaseText $ replaceMappings (head key) mappings) <> " IN " <> valueToTextForInConditions (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      [key'] -> quote' (textToSnakeCaseText $ replaceMappings key' mappings) <> getKeyConditionText (fromMaybe A.emptyObject (HM.lookup key' obj))
      _ -> ""
  _ -> ""

getKeyConditionText :: Value -> Text
getKeyConditionText values = case values of
  A.Object obj -> do
    let key = HM.keys obj
    case key of
      ["$gt"] -> " > " <> valueToText (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$gte"] -> " >= " <> valueToText (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$lt"] -> " < " <> valueToText (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$lte"] -> " <= " <> valueToText (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$ne"] -> " != " <> valueToText (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$notIn"] -> " NOT IN " <> valueToTextForInConditions (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      ["$in"] -> " IN " <> valueToTextForInConditions (fromMaybe A.emptyObject (HM.lookup (head key) obj))
      [key'] -> " = " <> valueToText (fromMaybe A.emptyObject (HM.lookup key' obj))
      _ -> ""
  A.String str -> " = " <> quote str
  A.Number num -> " = " <> quote (T.pack (show (Sci.coefficient num)))
  A.Bool bool' -> " = " <> quote (if bool' then "true" else "false")
  A.Null -> " IS NULL "
  _ -> ""

getArrayConditionText :: Value -> Text -> A.Object -> Text
getArrayConditionText arr cnd mappings = case arr of
  A.Array arr' -> case V.toList arr' of
    [] -> ""
    [x] -> makeWhereCondition x mappings
    (x : xs) -> "(" <> makeWhereCondition x mappings <> ")" <> cnd <> "(" <> getArrayConditionText (A.Array (V.fromList xs)) cnd mappings <> ")"
  _ -> ""

createInKafka :: Producer.KafkaProducer -> Value -> Text -> Text -> IO (Either Text ())
createInKafka producer dbObject dbStreamKey model = do
  let topicName = "driver-drainer-" <> T.pack (camel (T.unpack model)) <> "BPP"
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
          prValue = Just . LBS.toStrict $ encode event
        }

shouldPushToKafkaOnly :: Text -> [Text] -> Bool
shouldPushToKafkaOnly model _dontEnableDbTables = textToSnakeCaseText model `elem` _dontEnableDbTables || model `elem` _dontEnableDbTables

shouldPushToDbOnly :: Text -> [Text] -> Bool
shouldPushToDbOnly model _dontEnableDbTables = textToSnakeCaseText model `elem` _dontEnableDbTables || model `elem` _dontEnableDbTables

getObjectForKafka :: A.Object -> Maybe A.Object -> A.Value
getObjectForKafka obj mapp =
  A.Object $
    HM.fromList $
      map
        ( \(key, val) ->
            let newKey = textToSnakeCaseText key
             in case mapp >>= HM.lookup key of
                  Just mappedKey -> (textToSnakeCaseText (valueToText mappedKey), val)
                  Nothing -> (newKey, val)
        )
        $ HM.toList obj
