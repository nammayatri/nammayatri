module DBSync.Delete where

import Config.Env
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Extra (mapLeft)
import Data.Maybe (fromJust)
import Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Beam.Postgres (Pg, Postgres)
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.KVConnector.DBSync
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import EulerHS.Types (DBConfig)
import Kafka.Producer
import qualified Kafka.Producer as KafkaProd
import qualified Kafka.Producer as Producer
import qualified Kernel.Beam.Types as KBT
import Sequelize
import qualified "rider-app" Storage.DBModel as DBModel
import System.Timeout (timeout)
import Text.Casing
import Types.DBSync
import Types.DBSync.DBModel
import Types.Event as Event
import Utils.Utils

runDeleteCommands :: (DeleteDBCommand, ByteString) -> Text -> ReaderT Env EL.Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runDeleteCommands (cmd, val) dbStreamKey = do
  let dbConf = fromJust <$> EL.getOption KBT.PsqlDbCfg
  let DeleteDBCommand id _ _ _ _ dbDeleteObject = cmd
  withDBObjectContent @DBDeleteObjectContent dbDeleteObject $ \(DBDeleteObjectContent whereClause) -> do
    runDeleteInKafkaAndDb id val dbStreamKey whereClause =<< dbConf

-- | Delete entry in DB if KAFKA_PUSH key is set to false. Else Delete in both.
runDeleteInKafkaAndDb ::
  IsDBTable DBModel.RiderApp table =>
  EL.KVDBStreamEntryID ->
  ByteString ->
  Text ->
  Where Postgres table ->
  DBConfig Pg ->
  ReaderT Env EL.Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runDeleteInKafkaAndDb id value dbStreamKey whereClause dbConf = do
  isPushToKafka' <- EL.runIO isPushToKafka
  if not isPushToKafka'
    then runDelete id value dbStreamKey whereClause dbConf
    else do
      res <- runDeleteInKafka id value dbStreamKey whereClause dbConf
      either (\_ -> pure $ Left (UnexpectedError "Kafka Error", id)) (\_ -> runDelete id value dbStreamKey whereClause dbConf) res

-- If KAFKA_PUSH is false then entry will be there in DB Else Delete entry in Kafka only.
runDeleteInKafka ::
  forall (table :: TableK).
  IsDBTable DBModel.RiderApp table =>
  EL.KVDBStreamEntryID ->
  ByteString ->
  Text ->
  Where Postgres table ->
  DBConfig Pg ->
  ReaderT Env EL.Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runDeleteInKafka id value dbStreamKey whereClause dbConf = do
  let dbModel = showDBModel (Proxy @table)
  isPushToKafka' <- EL.runIO isPushToKafka
  if not isPushToKafka'
    then runDelete id value dbStreamKey whereClause dbConf
    else do
      Env {..} <- ask
      res <- EL.runIO $ streamRiderDrainerDeletes _kafkaConnection (getDbDeleteDataJson dbModel whereClause) dbStreamKey
      either
        ( \_ -> do
            void $ publishDBSyncMetric Event.KafkaPushFailure
            EL.logError ("ERROR:" :: Text) ("Kafka Create Error " :: Text)
            pure $ Left (UnexpectedError "Kafka Error", id)
        )
        (\_ -> pure $ Right id)
        res

runDelete ::
  forall (table :: TableK).
  IsDBTable DBModel.RiderApp table =>
  EL.KVDBStreamEntryID ->
  ByteString ->
  Text ->
  Where Postgres table ->
  DBConfig Pg ->
  ReaderT Env EL.Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runDelete id value _ whereClause dbConf = do
  maxRetries <- EL.runIO getMaxRetries
  Env {..} <- ask
  let dbModel = showDBModel (Proxy @table)
  if dbModel `elem` _dontEnableDbTables then pure $ Right id else runDeleteWithRetries id value whereClause dbConf 0 maxRetries

runDeleteWithRetries ::
  forall (table :: TableK).
  IsDBTable DBModel.RiderApp table =>
  EL.KVDBStreamEntryID ->
  ByteString ->
  Where Postgres table ->
  DBConfig Pg ->
  Int ->
  Int ->
  ReaderT Env EL.Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runDeleteWithRetries id value whereClause dbConf retryIndex maxRetries = do
  let dbModel = showDBModel (Proxy @table)
  res <- mapLeft MDBError <$> CDB.deleteAllReturning dbConf whereClause
  case (res, retryIndex) of
    (Left _, y) | y < maxRetries -> do
      void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Delete" dbModel
      EL.runIO $ delay =<< getRetryDelay
      runDeleteWithRetries id value whereClause dbConf (retryIndex + 1) maxRetries
    (Left x, _) -> do
      void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Delete" dbModel
      EL.logError (("Delete failed: " :: Text) <> T.pack (show x)) (show [("command" :: String, value)] :: Text)
      pure $ Left (x, id)
    (Right _, _) -> do
      pure $ Right id

streamRiderDrainerDeletes :: ToJSON a => Producer.KafkaProducer -> a -> Text -> IO (Either Text ())
streamRiderDrainerDeletes producer dbObject dbStreamKey = do
  let topicName = "rider-drainer"
  void $ KafkaProd.produceMessage producer (message topicName dbObject)
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
          prKey = Just $ TE.encodeUtf8 dbStreamKey,
          prValue = Just . LBS.toStrict $ encode event
        }

getDbDeleteDataJson :: forall be table. (Model be table, MeshMeta be table) => Text -> Where be table -> A.Value
getDbDeleteDataJson model whereClause =
  A.object
    [ "contents"
        .= A.object
          [ "where" .= modelEncodeWhere whereClause
          ],
      "tag" .= T.pack (pascal (T.unpack model)),
      "type" .= ("DELETE" :: Text)
    ]
