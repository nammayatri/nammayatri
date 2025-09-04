module DBSync.Update where

import Config.Env
import Control.Exception
import DBQuery.Functions
import DBQuery.Types
import Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Text as T hiding (map)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple.Types
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id, try)
import Kernel.Beam.Lib.Utils as KBLU
import Types.DBSync
import Types.Event as Event
import Utils.Utils

-- | This function is used to run the update operation for a single entry in the stream
runUpdate :: (EL.KVDBStreamEntryID, ByteString) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runUpdate updateDataEntries streamName = do
  Env {..} <- ask
  isPushToKafka' <- EL.runIO isPushToKafka
  let (entryId, streamData) = updateDataEntries
  EL.logDebug ("BYTE STRING" :: Text) (show streamData)
  case A.eitherDecode @DBUpdateObject . LBS.fromStrict $ streamData of
    Right updateDBModel -> do
      EL.logDebug ("DB OBJECT" :: Text) (show updateDBModel)
      let tableName = updateDBModel.dbModel
      if shouldPushToDbOnly tableName _dontEnableForKafka || not isPushToKafka'
        then runUpdateQuery updateDataEntries updateDBModel
        else do
          let updateObject = KBLU.replaceMappings (maybe (A.object []) A.Object (updateDBModel.updatedModel)) (HM.fromList . M.toList $ updateDBModel.mappings.getMapping)
          res <- EL.runIO $ createInKafka _kafkaConnection updateObject streamName tableName
          case res of
            Left err -> do
              EL.logError ("KAFKA UPDATE FAILED" :: Text) ("Kafka update failed for drainer : " <> err <> " for table :: " <> show tableName)
              void $ publishDBSyncMetric $ Event.KafkaPushFailure "Update" tableName.getDBModel
              _ <- runUpdateQuery updateDataEntries updateDBModel --- it should push that entry to db what if isForcePushEnabled is true then it will get missed
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("KAFKA UPDATE SUCCESSFUL" :: Text) (" Update successful for object :: " <> show updateDBModel.contents)
              runUpdateQuery updateDataEntries updateDBModel
    Left err -> do
      EL.logError ("UPDATE FAILED" :: Text) ("Invalid streamData or Extraction of data from redis stream failed :: " <> TE.decodeUtf8 streamData <> "; error :: " <> show err)
      return $ Left entryId

runUpdateQuery :: (EL.KVDBStreamEntryID, ByteString) -> DBUpdateObject -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runUpdateQuery updateDataEntries dbUpdateObject = do
  Env {..} <- ask
  let (entryId, byteString) = updateDataEntries
  let dbModel = dbUpdateObject.dbModel
  if shouldPushToKafkaOnly dbModel _dontEnableDbTables && not dbUpdateObject.forceDrainToDB
    then return $ Right entryId
    else do
      let updateQuery = getUpdateQueryForTable dbUpdateObject
      case updateQuery of
        Just query -> do
          result <- EL.runIO $ try $ executeQueryUsingConnectionPool _connectionPool (Query $ TE.encodeUtf8 query)
          case result of
            Left (QueryError errorMsg) -> do
              EL.logError ("QUERY UPDATE FAILED" :: Text) ("(ENTRY ID :: " <> show entryId <> ") => " <> errorMsg <> " for query :: " <> query)
              EL.logError ("QUERY UPDATE FAILED : BYTE STRING" :: Text) (TE.decodeUtf8 byteString)
              EL.logError ("QUERY UPDATE FAILED : DB OBJECT" :: Text) (show dbUpdateObject)
              void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" dbModel.getDBModel
              return $ Left entryId
            Right _ -> do
              EL.logDebug ("QUERY UPDATE SUCCESSFUL" :: Text) (" Update successful for query :: " <> query <> " with streamData :: " <> TE.decodeUtf8 byteString)
              return $ Right entryId
        Nothing -> do
          EL.logError ("No query generated for streamData: " :: Text) (TE.decodeUtf8 byteString)
          return $ Left entryId

getUpdateQueryForTable :: DBUpdateObject -> Maybe Text
getUpdateQueryForTable DBUpdateObject {dbModel, contents, mappings} = do
  let DBUpdateObjectContent setClauses whereClause = contents
  let schema = SchemaName $ T.pack currentSchemaName
  generateUpdateQuery UpdateQuery {..}
