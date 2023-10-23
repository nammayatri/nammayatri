module DBSync.Create where

import Config.Env
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text as T hiding (any, map, null)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Database.PostgreSQL.Simple.Types
import EulerHS.Language as EL
import EulerHS.Prelude
import Types.DBSync
import Utils.Utils

-- | This function is used to run the create operation for a single entry in the stream
runCreate :: (EL.KVDBStreamEntryID, ByteString) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runCreate createDataEntry streamName = do
  Env {..} <- ask
  isPushToKafka' <- EL.runIO isPushToKafka
  let (entryId, streamData) = createDataEntry
      (mbModel, mbObject, mbMappings) = getDataFromByteStringForCreate streamData
  case (mbModel, mbObject) of
    (Just model, Just obj) -> do
      let tableName = getModelname model 6 -- 6 because this string has "Object" appendend to it along with the model name
      if shouldPushToDbOnly tableName _dontEnableForKafka || not isPushToKafka'
        then runCreateQuery createDataEntry (mbModel, mbObject, mbMappings) tableName
        else do
          let createObject = getObjectForKafka obj mbMappings
          res <- EL.runIO $ createInKafka _kafkaConnection createObject streamName tableName
          case res of
            Left err -> do
              EL.logError ("KAFKA CREATE FAILED" :: Text) (err <> " for Object :: " <> show obj)
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("KAFKA CREATE SUCCESSFUL" :: Text) (" Create successful for object :: " <> show obj)
              runCreateQuery createDataEntry (mbModel, mbObject, mbMappings) tableName
    _ -> do
      EL.logError ("CREATE FAILED" :: Text) ("Invalid streamData or Extraction of data from redis stream failed :: " <> TE.decodeUtf8 streamData)
      return $ Left entryId

-- | Run a create query for a single entry in the stream
runCreateQuery :: (EL.KVDBStreamEntryID, ByteString) -> (Maybe Text, Maybe A.Object, Maybe A.Object) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runCreateQuery createDataEntry createData tableName = do
  Env {..} <- ask
  let (entryId, byteString) = createDataEntry
      (mbModel, mbObject, mbMappings) = createData
  if shouldPushToKafkaOnly tableName _dontEnableDbTables
    then return $ Right entryId
    else do
      let insertQuery = generateInsertForTable (mbModel, mbObject, mbMappings) tableName
      case insertQuery of
        Just query -> do
          result <- EL.runIO $ try $ executeQuery _pgConnection (Query $ TE.encodeUtf8 query)
          case result of
            Left (QueryError errorMsg) -> do
              EL.logError ("QUERY INSERT FAILED" :: Text) (errorMsg <> " for query :: " <> query)
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("QUERY INSERT SUCCESSFUL" :: Text) (" Insert successful for query :: " <> query <> " with streamData :: " <> TE.decodeUtf8 byteString)
              return $ Right entryId
        Nothing -> do
          EL.logError ("No query generated for streamData: " :: Text) (TE.decodeUtf8 byteString)
          return $ Left entryId

-- | Generate an insert query for the atlas_driver_offer_bpp schema
generateInsertForTable :: (Maybe Text, Maybe A.Object, Maybe A.Object) -> Text -> Maybe Text
generateInsertForTable (mbModel, mbObject, mbMappings) tableName =
  case (mbModel, mbObject) of
    (Just _, Just object') -> do
      let keys' = HM.keys object'
          newKeys = map (`replaceMappings` fromMaybe HM.empty mbMappings) keys'
          newKeys' = map (quote' . textToSnakeCaseText) newKeys
          values = map (valueToText . fromMaybe A.Null . (`HM.lookup` object')) keys'
          table = "atlas_driver_offer_bpp." <> quote' tableName
          inserts = T.intercalate ", " newKeys'
          valuesList = T.intercalate ", " values
      Just $ "INSERT INTO " <> table <> " (" <> inserts <> ") VALUES (" <> valuesList <> ")" <> " ON CONFLICT DO NOTHING;"
    _ -> Nothing

-- This function is used to extract the model name, object and mappings from the stream data
getDataFromByteStringForCreate :: ByteString -> (Maybe Text, Maybe A.Object, Maybe A.Object)
getDataFromByteStringForCreate dbCommandByteString =
  case A.decodeStrict dbCommandByteString of
    Just (A.Object o) ->
      case HM.lookup "contents" o of
        Just (A.Array a) ->
          case V.last a of
            (A.Object obj) -> (lookupText "tag" obj, lookupObject "contents" obj, lookupObject "mappings" obj)
            _ -> (Nothing, Nothing, Nothing)
        _ -> (Nothing, Nothing, Nothing)
    _ -> (Nothing, Nothing, Nothing)
