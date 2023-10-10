module DBSync.Create where

import Config.Env
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text.Encoding as TE
import Database.Beam.Postgres (Pg, Postgres)
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.Language as EL
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types as ET
import Kafka.Producer as KafkaProd
import Kafka.Producer as Producer
import qualified Kernel.Beam.Types as KBT
import qualified "dynamic-offer-driver-app" Storage.DBModel as DBModel
import System.Timeout (timeout)
import Types.DBSync
import Types.DBSync.DBModel
import Types.Event as Event
import Utils.Utils

data CreateObject b table = CreateObject
  { dbObject :: table Identity,
    bts :: b,
    entryId :: KVDBStreamEntryID,
    kafkaObject :: DBCreateObject
  }

runCreateCommands :: Show b => [(CreateDBCommand, b)] -> Text -> ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateCommands cmds streamKey = do
  dbConf <- fromJust <$> L.getOption KBT.PsqlDbCfg
  let dbObjectsWithPayload = cmds <&> \payload@(CreateDBCommand _ _ _ _ _ dbObj, _) -> (dbObj, payload)
  foldl1 (|::|) $
    availableDBModels <&> \dbModel -> do
      withFilteredDBObjectContent dbModel dbObjectsWithPayload $ \filteredObjectsWithPayload -> do
        let createObjects =
              filteredObjectsWithPayload <&> \(DBCreateObjectContent dbObject, (CreateDBCommand entryId _ _ _ _ dbCreateObject, b)) -> do
                CreateObject
                  { dbObject,
                    bts = b,
                    entryId,
                    kafkaObject = dbCreateObject
                  }
        case dbModel of
          DBModel.BecknRequest -> runCreate dbConf createObjects
          _ -> runCreateInKafkaAndDb dbConf streamKey createObjects

-- | Create entry in DB if KAFKA_PUSH key is set to false. Else creates in both.
runCreateInKafkaAndDb ::
  (Show b, IsDBTable DBModel.DriverApp table) =>
  DBConfig Pg ->
  Text ->
  [CreateObject b table] ->
  ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateInKafkaAndDb dbConf streamKey object = do
  isPushToKafka' <- EL.runIO isPushToKafka
  if not isPushToKafka'
    then runCreate dbConf object
    else
      if null object
        then pure [Right []]
        else do
          let entryIds = object <&> (.entryId)
          kResults <- runCreateInKafka dbConf streamKey object
          case kResults of
            [Right _] -> runCreate dbConf object
            _ -> pure [Left entryIds]

-- | If KAFKA_PUSH is false then entry will be there in DB Else Create entry in Kafka only.
runCreateInKafka ::
  forall (table :: (Type -> Type) -> Type) b.
  (Show b, IsDBTable DBModel.DriverApp table) =>
  DBConfig Pg ->
  Text ->
  [CreateObject b table] ->
  ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateInKafka dbConf streamKey object = do
  isPushToKafka' <- EL.runIO isPushToKafka
  if not isPushToKafka'
    then runCreate dbConf object -- why both runCreateInKafkaAndDb and runCreateInKafka call runCreate?
    else
      if null object
        then pure [Right []]
        else do
          let dataObjects = object <&> (.kafkaObject)
              entryIds = object <&> (.entryId)
          Env {..} <- ask
          res <- EL.runIO $ streamDriverDrainerCreates _kafkaConnection dataObjects streamKey
          either
            ( \_ -> do
                void $ publishDBSyncMetric Event.KafkaPushFailure
                EL.logError ("ERROR:" :: Text) ("Kafka Create Error " :: Text)
                pure [Left entryIds]
            )
            (\_ -> pure [Right entryIds])
            res

runCreate ::
  forall (table :: (Type -> Type) -> Type) b.
  (Show b, IsDBTable DBModel.DriverApp table) =>
  DBConfig Pg ->
  [CreateObject b table] ->
  ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreate dbConf object = do
  let dbObjects = object <&> (.dbObject)
      byteStream = object <&> (.bts)
      entryIds = object <&> (.entryId)
      cmdsToErrorQueue = map ("command" :: String,) byteStream
  Env {..} <- ask
  maxRetries <- EL.runIO getMaxRetries
  let dbModel = showDBModel (Proxy @table)
  if null object || dbModel `elem` _dontEnableDbTables then pure [Right []] else runCreateWithRecursion dbConf dbObjects cmdsToErrorQueue entryIds 0 maxRetries False

runCreateWithRecursion ::
  forall (table :: (Type -> Type) -> Type) b.
  (Show b, IsDBTable DBModel.DriverApp table) =>
  DBConfig Pg ->
  [table Identity] ->
  [(String, b)] ->
  [KVDBStreamEntryID] ->
  Int ->
  Int ->
  Bool ->
  ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateWithRecursion dbConf dbObjects cmdsToErrorQueue entryIds index maxRetries ignoreDuplicates = do
  let dbModel = showDBModel (Proxy @table)
  res <- CDB.createMultiSqlWoReturning @Postgres @Pg dbConf dbObjects ignoreDuplicates
  case (res, index) of -- Ignore duplicate entry
    (Right _, _) -> do
      -- EL.logInfoV ("Drainer Info" :: Text) $ createDBLogEntry model "CREATE" (t2 - t1) (cpuT2 - cpuT1) dbObjects -- Logging group latencies
      pure [Right entryIds]
    (Left (ET.DBError (ET.SQLError (ET.MysqlError (ET.MysqlSqlError 1062 err))) _), _) -> do
      EL.logInfo ("DUPLICATE_ENTRY" :: Text) ("Got duplicate entry for model: " <> dbModel <> ", Error message: " <> err)
      void $ publishDBSyncMetric $ Event.DuplicateEntryCreate dbModel
      -- Is retry delay needed here? :/
      runCreateWithRecursion dbConf dbObjects cmdsToErrorQueue entryIds index maxRetries True -- Should retry count be increased here? :/
    (Left (ET.DBError (ET.SQLError (ET.PostgresError (ET.PostgresSqlError ("23505" :: Text) _ errMsg _ _))) _), _) -> do
      EL.logInfo ("DUPLICATE_ENTRY" :: Text) ("Got duplicate entry for model: " <> dbModel <> ", Error message: " <> errMsg)
      void $ publishDBSyncMetric $ Event.DuplicateEntryCreate dbModel
      -- Is retry delay needed here? :/
      runCreateWithRecursion dbConf dbObjects cmdsToErrorQueue entryIds index maxRetries True -- Should retry count be increased here? :/
    (Left _, y) | y < maxRetries -> do
      void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" dbModel -- FIXME use DBModel type everywhere
      EL.runIO $ delay =<< getRetryDelay
      runCreateWithRecursion dbConf dbObjects cmdsToErrorQueue entryIds (index + 1) maxRetries ignoreDuplicates -- Should we pass the same ignoreDuplicates or should we pass False here.
    (Left x, _) -> do
      void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" dbModel
      EL.logError ("Create failed: " :: Text) (show cmdsToErrorQueue <> "\n Error: " <> show x :: Text)
      pure [Left entryIds]

streamDriverDrainerCreates :: ToJSON a => Producer.KafkaProducer -> [a] -> Text -> IO (Either Text ())
streamDriverDrainerCreates producer dbObject streamKey = do
  let topicName = "driver-drainer"
  mapM_ (KafkaProd.produceMessage producer . message topicName) dbObject
  flushResult <- timeout (5 * 60 * 1000000) $ prodPush producer
  case flushResult of
    Just _ -> do
      pure $ Right ()
    Nothing -> pure $ Left "KafkaProd.flushProducer timed out after 5 minutes"
  where
    prodPush producer' = KafkaProd.flushProducer producer' >> pure True

    message topicName event =
      ProducerRecord
        { prTopic = TopicName topicName,
          prPartition = UnassignedPartition,
          prKey = Just $ TE.encodeUtf8 streamKey,
          prValue = Just . LBS.toStrict $ encode event
        }
