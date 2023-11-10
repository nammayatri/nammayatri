module DBSync.Delete where

import Config.Env
import DBQuery.Functions
import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple.Types (Query (..))
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import Text.Casing (pascal)
import "rider-app" Tools.Beam.UtilsTH (currentSchemaName)
import Types.DBSync
import Utils.Utils

-- | This function is used to run the delete operation for a single entry in the stream
runDelete :: (EL.KVDBStreamEntryID, ByteString) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runDelete deleteEntry streamName = do
  Env {..} <- ask
  isPushToKafka' <- EL.runIO isPushToKafka
  let (entryId, streamData) = deleteEntry
  EL.logDebug ("BYTE STRING" :: Text) (show streamData)
  case A.eitherDecode @DBDeleteObject . LBS.fromStrict $ streamData of
    Right deleteDBModel -> do
      EL.logDebug ("DB OBJECT" :: Text) (show deleteDBModel)
      let tableName = deleteDBModel.dbModel
      if shouldPushToDbOnly tableName _dontEnableForKafka || not isPushToKafka'
        then runDeleteQuery deleteEntry deleteDBModel
        else do
          let deleteObject = getDbDeleteDataJson tableName deleteDBModel.whereObj
          res <- EL.runIO $ createInKafka _kafkaConnection deleteObject streamName tableName
          case res of
            Left err -> do
              EL.logError ("KAFKA DELETE FAILED" :: Text) (err <> " for Object :: " <> show deleteDBModel.contents)
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("KAFKA DELETE SUCCESSFUL" :: Text) (" Delete successful for object :: " <> show deleteDBModel.contents)
              runDeleteQuery deleteEntry deleteDBModel
    _ -> do
      EL.logError ("DELETE FAILED" :: Text) ("Invalid streamData or Extraction of data from redis stream failed :: " <> TE.decodeUtf8 streamData)
      return $ Left entryId

runDeleteQuery :: (EL.KVDBStreamEntryID, ByteString) -> DBDeleteObject -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runDeleteQuery deleteEntries dbDeleteObject = do
  Env {..} <- ask
  let (entryId, byteString) = deleteEntries
  let dbModel = dbDeleteObject.dbModel
  if shouldPushToKafkaOnly dbModel _dontEnableDbTables
    then return $ Right entryId
    else do
      let deleteQuery = getDeleteQueryForTable dbDeleteObject
      case deleteQuery of
        Just query -> do
          EL.logDebug ("QUERY" :: Text) query
          result <- EL.runIO $ try $ executeQuery _pgConnection (Query $ TE.encodeUtf8 query)
          case result of
            Left (QueryError errorMsg) -> do
              EL.logError ("QUERY DELETE FAILED" :: Text) (errorMsg <> " for query :: " <> query)
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("QUERY DELETE SUCCESSFUL" :: Text) (" Delete successful for query :: " <> query <> " with streamData :: " <> TE.decodeUtf8 byteString)
              return $ Right entryId
        Nothing -> do
          EL.logError ("No query generated for streamData: " :: Text) (TE.decodeUtf8 byteString)
          return $ Left entryId

getDeleteQueryForTable :: DBDeleteObject -> Maybe Text
getDeleteQueryForTable DBDeleteObject {dbModel, contents, mappings} = do
  let DBDeleteObjectContent whereClause = contents
  let schema = SchemaName $ T.pack currentSchemaName
  generateDeleteQuery DeleteQuery {..}

getDbDeleteDataJson :: DBModel -> A.Value -> A.Value
getDbDeleteDataJson model whereClause =
  A.object
    [ "contents"
        A..= whereClause,
      "tag" A..= ((T.pack . pascal . T.unpack) model.getDBModel <> "Object"),
      "type" A..= ("DELETE" :: Text)
    ]
