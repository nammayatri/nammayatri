module DBSync.Create where

import Config.Env
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust)
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
import qualified Sequelize
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
        runCreateInKafkaAndDb dbConf streamKey dbModel createObjects

-- | Create entry in DB if KAFKA_PUSH key is set to false. Else creates in both.
runCreateInKafkaAndDb ::
  forall (table :: (Type -> Type) -> Type) b.
  (Show b, Sequelize.Model Postgres table) =>
  DBConfig Pg ->
  Text ->
  DBModel ->
  [CreateObject b table] ->
  ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateInKafkaAndDb dbConf streamKey' model object = do
  isPushToKafka' <- EL.runIO isPushToKafka
  if not isPushToKafka'
    then runCreate dbConf streamKey' model object
    else
      if null object
        then pure [Right []]
        else do
          let entryIds = object <&> (.entryId)
          kResults <- runCreateInKafka dbConf streamKey' model object
          case kResults of
            [Right _] -> runCreate dbConf streamKey' model object
            _ -> pure [Left entryIds]

-- | If KAFKA_PUSH is false then entry will be there in DB Else Create entry in Kafka only.
runCreateInKafka ::
  forall (table :: (Type -> Type) -> Type) b.
  (Show b, Sequelize.Model Postgres table) =>
  Show b =>
  DBConfig Pg ->
  Text ->
  DBModel ->
  [CreateObject b table] ->
  ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateInKafka dbConf streamKey' model object = do
  isPushToKafka' <- EL.runIO isPushToKafka
  if not isPushToKafka'
    then runCreate dbConf streamKey' model object -- why both runCreateInKafkaAndDb and runCreateInKafka call runCreate?
    else
      if null object
        then pure [Right []]
        else do
          let dataObjects = object <&> (.kafkaObject)
              entryIds = object <&> (.entryId)
          Env {..} <- ask
          res <- EL.runIO $ streamRiderDrainerCreates _kafkaConnection dataObjects streamKey'
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
  (Show b, Sequelize.Model Postgres table) =>
  Show b =>
  DBConfig Pg ->
  Text ->
  DBModel ->
  [CreateObject b table] ->
  ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreate dbConf _ model object = do
  let dbObjects = object <&> (.dbObject)
      byteStream = object <&> (.bts)
      entryIds = object <&> (.entryId)
      cmdsToErrorQueue = map ("command" :: String,) byteStream
  maxRetries <- EL.runIO getMaxRetries
  if null object then pure [Right []] else runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds 0 maxRetries False

runCreateWithRecursion ::
  forall (table :: (Type -> Type) -> Type) b.
  (Show b, Sequelize.Model Postgres table) =>
  Show b =>
  DBConfig Pg ->
  DBModel ->
  [table Identity] ->
  [(String, b)] ->
  [KVDBStreamEntryID] ->
  Int ->
  Int ->
  Bool ->
  ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds index maxRetries ignoreDuplicates = do
  res <- CDB.createMultiSqlWoReturning @Postgres @Pg dbConf dbObjects ignoreDuplicates -- what about multiple objects?
  case (res, index) of
    (Right _, _) -> do
      pure [Right entryIds]
    (Left (ET.DBError (ET.SQLError (ET.MysqlError (ET.MysqlSqlError 1062 err))) _), _) -> do
      EL.logInfo ("DUPLICATE_ENTRY" :: Text) ("Got duplicate entry for model: " <> show model <> ", Error message: " <> err)
      void $ publishDBSyncMetric $ Event.DuplicateEntryCreate $ show model
      runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds index maxRetries True
    (Left (ET.DBError (ET.SQLError (ET.PostgresError (ET.PostgresSqlError ("23505" :: Text) _ errMsg _ _))) _), _) -> do
      EL.logInfo ("DUPLICATE_ENTRY" :: Text) ("Got duplicate entry for model: " <> show model <> ", Error message: " <> errMsg)
      void $ publishDBSyncMetric $ Event.DuplicateEntryCreate $ show model
      runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds index maxRetries True
    (Left _, y) | y < maxRetries -> do
      void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" $ show model -- FIXME use DBModel type everywhere
      EL.runIO $ delay =<< getRetryDelay
      runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds (index + 1) maxRetries ignoreDuplicates
    (Left x, _) -> do
      void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" $ show model
      EL.logError ("Create failed: " :: Text) (show cmdsToErrorQueue <> "\n Error: " <> show x :: Text)
      pure [Left entryIds]

-- TODO reuse code from rider and driver drainer. All the same except topicName
streamRiderDrainerCreates :: ToJSON a => Producer.KafkaProducer -> [a] -> Text -> IO (Either Text ())
streamRiderDrainerCreates producer dbObject streamKey = do
  let topicName = "rider-drainer"
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
