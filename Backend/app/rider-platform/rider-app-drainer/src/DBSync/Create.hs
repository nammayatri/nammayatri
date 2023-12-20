module DBSync.Create where

import Config.Env
import DBQuery.Functions
import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text as T hiding (any, map, null)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple.Types
import EulerHS.Language as EL
import EulerHS.Prelude
import Kernel.Beam.Lib.Utils as KBLU
import Text.Casing (pascal)
import "rider-app" Tools.Beam.UtilsTH (currentSchemaName)
import Types.DBSync
import Types.Event as Event
import Utils.Utils

-- | This function is used to run the create operation for a single entry in the stream
runCreate :: (EL.KVDBStreamEntryID, ByteString) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runCreate createDataEntry streamName = do
  Env {..} <- ask
  isPushToKafka' <- EL.runIO isPushToKafka
  let (entryId, streamData) = createDataEntry
  EL.logDebug ("BYTE STRING" :: Text) (show streamData)
  case A.eitherDecode @DBCreateObject . LBS.fromStrict $ streamData of
    Right createDBModel -> do
      EL.logDebug ("DB OBJECT" :: Text) (show createDBModel)
      let tableName = createDBModel.dbModel
      -- uncomment for debug purposes
      -- writeDebugFile "create" tableName entryId "streamData.json" streamData
      -- writeDebugFile "create" tableName entryId "dbObject.txt" $ show createDBModel
      if shouldPushToDbOnly tableName _dontEnableForKafka || not isPushToKafka'
        then runCreateQuery createDataEntry createDBModel
        else do
          let createObject = KBLU.replaceMappings (A.Object createDBModel.contentsObj) (HM.fromList . M.toList $ createDBModel.mappings.getMapping)
          res <- EL.runIO $ createInKafka _kafkaConnection createObject streamName tableName
          case res of
            Left err -> do
              EL.logError ("KAFKA CREATE FAILED" :: Text) (err <> " for Object :: " <> show createDBModel.contents)
              void $ publishDBSyncMetric Event.KafkaPushFailure
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("KAFKA CREATE SUCCESSFUL" :: Text) (" Create successful for object :: " <> show createDBModel.contents)
              runCreateQuery createDataEntry createDBModel
    Left err -> do
      EL.logError ("CREATE FAILED" :: Text) ("Invalid streamData or Extraction of data from redis stream failed :: " <> TE.decodeUtf8 streamData <> "; error :: " <> show err)
      return $ Left entryId

-- | Run a create query for a single entry in the stream
runCreateQuery :: (EL.KVDBStreamEntryID, ByteString) -> DBCreateObject -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runCreateQuery createDataEntry dbCreateObject = do
  Env {..} <- ask
  let (entryId, byteString) = createDataEntry
      dbModel = dbCreateObject.dbModel
  if shouldPushToKafkaOnly dbModel _dontEnableDbTables
    then return $ Right entryId
    else do
      let insertQuery = generateInsertForTable dbCreateObject
      case insertQuery of
        Just query -> do
          EL.logDebug ("QUERY" :: Text) query -- TODO redundant
          result <- EL.runIO $ try $ executeQuery _pgConnection (Query $ TE.encodeUtf8 query)
          case result of
            Left (QueryError errorMsg) -> do
              EL.logError ("QUERY INSERT FAILED" :: Text) (errorMsg <> " for query :: " <> query)
              void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" dbModel.getDBModel
              -- uncomment for debug purposes
              -- writeDebugFile "create" dbModel entryId "queryFailed.sql" $ encodeUtf8 query
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("QUERY INSERT SUCCESSFUL" :: Text) (" Insert successful for query :: " <> query <> " with streamData :: " <> TE.decodeUtf8 byteString)
              -- uncomment for debug purposes
              -- writeDebugFile "create" dbModel entryId "querySuccessful.sql" $ encodeUtf8 query
              return $ Right entryId
        Nothing -> do
          EL.logError ("No query generated for streamData: " :: Text) (TE.decodeUtf8 byteString)
          return $ Left entryId

-- | Generate an insert query for the rider_app schema
generateInsertForTable :: DBCreateObject -> Maybe Text
generateInsertForTable DBCreateObject {dbModel, contents, mappings} = do
  let DBCreateObjectContent termWarps = contents
  let schema = SchemaName $ T.pack currentSchemaName
  generateInsertQuery InsertQuery {..}

getCreateObjectForKafka :: DBModel -> A.Object -> A.Value
getCreateObjectForKafka model content =
  A.object
    [ "contents" A..= content,
      "tag" A..= ((T.pack . pascal . T.unpack) model.getDBModel <> "Object"),
      "type" A..= ("INSERT" :: Text)
    ]
