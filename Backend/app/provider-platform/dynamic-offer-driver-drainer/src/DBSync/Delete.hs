module DBSync.Delete where

import Config.Env
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T hiding (elem)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Database.PostgreSQL.Simple.Types (Query (..))
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import Text.Casing (pascal)
import Types.DBSync
import Utils.Utils

-- | This function is used to run the delete operation for a single entry in the stream
runDelete :: (EL.KVDBStreamEntryID, ByteString) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runDelete deleteEntry streamName = do
  Env {..} <- ask
  isPushToKafka' <- EL.runIO isPushToKafka
  let (entryId, streamData) = deleteEntry
      (mbModel, mbObject, mbMappings) = getDataFromByteStringForDelete streamData
  case (mbModel, mbObject) of
    (Just model, Just obj) -> do
      let tableName = getModelname model 7 -- 7 because "Options" is appended to the model name
      if shouldPushToDbOnly tableName _dontEnableForKafka || not isPushToKafka'
        then runDeleteQuery deleteEntry (mbModel, mbObject, mbMappings) tableName
        else do
          let deleteObject = getDbDeleteDataJson model obj
          res <- EL.runIO $ createInKafka _kafkaConnection deleteObject streamName tableName
          case res of
            Left err -> do
              EL.logError ("KAFKA DELETE FAILED" :: Text) (err <> " for Object :: " <> show obj)
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("KAFKA DELETE SUCCESSFUL" :: Text) (" Delete successful for object :: " <> show obj)
              runDeleteQuery deleteEntry (mbModel, mbObject, mbMappings) tableName
    _ -> do
      EL.logError ("DELETE FAILED" :: Text) ("Invalid streamData or Extraction of data from redis stream failed :: " <> TE.decodeUtf8 streamData)
      return $ Left entryId

runDeleteQuery :: (EL.KVDBStreamEntryID, ByteString) -> (Maybe Text, Maybe A.Value, Maybe A.Object) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runDeleteQuery deleteEntries deleteData tableName = do
  Env {..} <- ask
  let (entryId, byteString) = deleteEntries
      (mbModel, mbObject, mbMappings) = deleteData
  if shouldPushToKafkaOnly tableName _dontEnableDbTables
    then return $ Right entryId
    else do
      let deleteQuery = getDeleteQueryForTable (mbModel, mbObject, mbMappings) tableName
      case deleteQuery of
        Just query -> do
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

getDeleteQueryForTable :: (Maybe Text, Maybe A.Value, Maybe A.Object) -> Text -> Maybe Text
getDeleteQueryForTable (mbModel, mbwhereVal, mbMappings) tableName = do
  case (mbModel, mbwhereVal) of
    (Just _, Just mbWhereVal') -> do
      let whereClause = makeWhereCondition mbWhereVal' (fromMaybe HM.empty mbMappings)
      pure $ "DELETE FROM atlas_driver_offer_bpp." <> quote' tableName <> " WHERE " <> whereClause <> ";"
    _ -> Nothing

-- This function is used to extract the model name, object and mappings from the stream data
getDataFromByteStringForDelete :: ByteString -> (Maybe T.Text, Maybe A.Value, Maybe A.Object)
getDataFromByteStringForDelete dbCommandByteString = do
  case A.decode $ LBS.fromStrict dbCommandByteString of
    Just (A.Object o) -> case HM.lookup "contents" o of
      Just (A.Array a) -> case V.last a of
        (A.Object obj') -> case HM.lookup "contents" obj' of
          Just (A.Object obj) -> (lookupText "tag" obj', HM.lookup "value1" obj, lookupObject "mappings" o)
          _ -> (Nothing, Nothing, Nothing)
        _ -> (Nothing, Nothing, Nothing)
      _ -> (Nothing, Nothing, Nothing)
    _ -> (Nothing, Nothing, Nothing)

getDbDeleteDataJson :: Text -> A.Value -> A.Value
getDbDeleteDataJson model whereClause =
  A.object
    [ "contents"
        A..= whereClause,
      "tag" A..= ((T.pack . pascal . T.unpack) (T.take (T.length model - 7) model) <> "Object"),
      "type" A..= ("DELETE" :: Text)
    ]
