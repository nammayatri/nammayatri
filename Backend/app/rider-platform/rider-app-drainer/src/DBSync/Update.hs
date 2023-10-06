module DBSync.Update where

import Config.Env
import Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Extra (mapLeft)
import Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import qualified Data.Serialize as Serialize
import Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Beam as B hiding (runUpdate)
import Database.Beam.Postgres (Pg, Postgres)
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.KVConnector.Types as EKT
import EulerHS.KVConnector.Utils as Utils
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import EulerHS.Types as ET
import Kafka.Producer as KafkaProd
import Kafka.Producer as Producer
import qualified Kernel.Beam.Functions as BeamFunction
import qualified Kernel.Beam.Types as KBT
import Sequelize (Model, Set, Where)
import qualified "rider-app" Storage.DBModel as DBModel
import System.Timeout (timeout)
import Text.Casing
import Types.DBSync
import Types.DBSync.DBModel
import Types.Event as Event
import Utils.Utils

updateDB ::
  forall beM be table m.
  ( HasCallStack,
    ET.BeamRuntime be beM,
    ET.BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    EL.MonadFlow m,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity)
  ) =>
  ET.DBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  ByteString ->
  m (Either MeshError ())
updateDB dbConf _ setClause whereClause _ =
  do
    either (pure . Left) (pure . Right) . mapLeft MDBError
    =<< CDB.updateOneWoReturning dbConf Nothing setClause whereClause

getUpdatedValue ::
  forall beM be table m.
  ( HasCallStack,
    ET.BeamRuntime be beM,
    ET.BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    EL.MonadFlow m,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity)
  ) =>
  Text ->
  Where be table ->
  m (Either MeshError (table Identity))
getUpdatedValue tag _ = do
  res <- EL.runKVDB BeamFunction.meshConfig.kvRedis $ EL.get $ fromString $ T.unpack tag
  case res of
    Right (Just r) -> do
      let (decodeResult :: MeshResult [table Identity], _isLive) = Utils.decodeToField $ BSL.fromChunks [r]
       in case decodeResult of
            Right [decodeRes] -> return $ Right decodeRes
            Right _ -> return $ Left (UnexpectedError "Redis Error: No Data for the key")
            Left _ -> return $ Left (UnexpectedError "Redis Error: Decode Failed")
    _ -> return $ Left (UnexpectedError "Redis Error")

runUpdateCommands :: (UpdateDBCommand, ByteString) -> Text -> Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runUpdateCommands (cmd, val) streamKey = do
  let dbConf = fromJust <$> EL.getOption KBT.PsqlDbCfg
  let UpdateDBCommand id _ tag _ _ dbUpdateObject = cmd
  withDBObjectContent dbUpdateObject $ \(DBUpdateObjectContent (setClauses :: [Set Postgres table]) whereClause) -> do
    case getDBModel (Proxy @DBModel.RiderApp) (Proxy @table) of
      DBModel.BecknRequest -> runUpdate id val streamKey setClauses whereClause =<< dbConf
      _ -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause =<< dbConf

-- Updates entry in DB if KAFKA_PUSH key is set to false. Else Updates in both.
runUpdateInKafkaAndDb ::
  IsDBTable DBModel.RiderApp table =>
  EL.KVDBStreamEntryID ->
  ByteString ->
  Text ->
  [Set Postgres table] ->
  Text ->
  Where Postgres table ->
  DBConfig Pg ->
  ReaderT Env EL.Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runUpdateInKafkaAndDb id value dbStreamKey' setClause tag whereClause dbConf = do
  isPushToKafka' <- EL.runIO isPushToKafka
  if not isPushToKafka'
    then runUpdate id value dbStreamKey' setClause whereClause dbConf
    else do
      res <- runUpdateInKafka id value dbStreamKey' setClause whereClause dbConf tag
      either (\_ -> pure $ Left (UnexpectedError "Kafka Error", id)) (\_ -> runUpdate id value dbStreamKey' setClause whereClause dbConf) res

-- If KAFKA_PUSH is false then entry will be there in DB Else Updates entry in Kafka only.
runUpdateInKafka ::
  forall (table :: TableK).
  IsDBTable DBModel.RiderApp table =>
  EL.KVDBStreamEntryID ->
  ByteString ->
  Text ->
  [Set Postgres table] ->
  Where Postgres table ->
  DBConfig Pg ->
  Text ->
  ReaderT Env EL.Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runUpdateInKafka id value dbStreamKey' setClause whereClause dbConf tag = do
  let dbModel = showDBModel (Proxy @table)
  isPushToKafka' <- EL.runIO isPushToKafka
  if not isPushToKafka'
    then runUpdate id value dbStreamKey' setClause whereClause dbConf
    else do
      res <- getUpdatedValue tag whereClause
      case res of
        Right dataObj -> do
          Env {..} <- ask
          let updatedJSON = getDbUpdateDataJson dbModel dataObj
          res'' <- EL.runIO $ streamRiderDrainerUpdates _kafkaConnection updatedJSON dbStreamKey'
          either
            ( \_ -> do
                void $ publishDBSyncMetric Event.KafkaPushFailure
                EL.logError ("ERROR:" :: Text) ("Kafka Rider Update Error " :: Text)
                pure $ Left (UnexpectedError "Kafka Rider Update Error", id)
            )
            (\_ -> pure $ Right id)
            res''
        Left _ -> do
          let updatedJSON = getDbUpdateDataJson dbModel $ updValToJSON $ jsonKeyValueUpdates setClause <> getPKeyandValuesList tag
          Env {..} <- ask
          res'' <- EL.runIO $ streamRiderDrainerUpdates _kafkaConnection updatedJSON dbStreamKey'
          either
            ( \_ -> do
                void $ publishDBSyncMetric Event.KafkaPushFailure
                EL.logError ("ERROR:" :: Text) ("Kafka Rider Update Error " :: Text)
                pure $ Left (UnexpectedError "Kafka Rider Update Error", id)
            )
            (\_ -> pure $ Right id)
            res''

runUpdate ::
  forall (table :: TableK).
  IsDBTable DBModel.RiderApp table =>
  EL.KVDBStreamEntryID ->
  ByteString ->
  Text ->
  [Set Postgres table] ->
  Where Postgres table ->
  DBConfig Pg ->
  ReaderT Env EL.Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runUpdate id value _ setClause whereClause dbConf = do
  maxRetries <- EL.runIO getMaxRetries
  Env {..} <- ask
  let dbModel = showDBModel (Proxy @table)
  if dbModel `elem` _dontEnableDbTables then pure $ Right id else runUpdateWithRetries id value setClause whereClause dbConf 0 maxRetries

-- TODO test: show AppInstall :: Text = "AppInstall"

runUpdateWithRetries ::
  forall (table :: TableK).
  IsDBTable DBModel.RiderApp table =>
  EL.KVDBStreamEntryID ->
  ByteString ->
  [Set Postgres table] ->
  Where Postgres table ->
  DBConfig Pg ->
  Int ->
  Int ->
  ReaderT Env EL.Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runUpdateWithRetries id value setClause whereClause dbConf retryIndex maxRetries = do
  let dbModel = showDBModel (Proxy @table)
  res <- updateDB dbConf Nothing setClause whereClause value
  case (res, retryIndex) of
    (Left _, y) | y < maxRetries -> do
      void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" dbModel
      EL.runIO $ delay =<< getRetryDelay
      runUpdateWithRetries id value setClause whereClause dbConf (retryIndex + 1) maxRetries
    (Left _, _) -> do
      void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" dbModel
      EL.logError (("Update failed for model: " :: Text) <> dbModel) (show [("command" :: String, value)] :: Text)
      pure $ Left (UnexpectedError "Update failed for model", id)
    (Right _, _) -> do
      pure $ Right id

streamRiderDrainerUpdates :: ToJSON a => Producer.KafkaProducer -> a -> Text -> IO (Either Text ())
streamRiderDrainerUpdates producer dbObject dbStreamKey = do
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

getDbUpdateDataJson :: ToJSON a => Text -> a -> A.Value
getDbUpdateDataJson model a =
  A.object
    [ "contents"
        .= A.toJSON a,
      "tag" .= T.pack (pascal (T.unpack model) <> "Object"),
      "type" .= ("UPDATE" :: Text)
    ]

updValToJSON :: [(Text, A.Value)] -> A.Value
updValToJSON keyValuePairs = A.Object $ HM.fromList keyValuePairs

getPKeyandValuesList :: Text -> [(Text, A.Value)]
getPKeyandValuesList pKeyAndValue = go (splitOn "_" pKeyTrimmed) []
  where
    go (tName : k : v : rest) acc = go (tName : rest) ((k, A.String v) : acc)
    go _ acc = acc
    pKeyTrimmed = case splitOn "{" pKeyAndValue of
      [] -> ""
      (x : _) -> x
