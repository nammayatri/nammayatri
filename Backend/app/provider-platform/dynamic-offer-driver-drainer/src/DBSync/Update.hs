module DBSync.Update where

import Config.Env
import Control.Exception
import Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.Text as T hiding (map)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Database.PostgreSQL.Simple.Types
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id, try)
import Types.DBSync
import Utils.Utils

-- | This function is used to run the update operation for a single entry in the stream
rundUpdate :: (EL.KVDBStreamEntryID, ByteString) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
rundUpdate updateDataEntries streamName = do
  Env {..} <- ask
  isPushToKafka' <- EL.runIO isPushToKafka
  let (entryId, streamData) = updateDataEntries
      (mbModel, mbObject, mbMappings, mbModelObject) = getDataFromByteStringForUpdate streamData
  case (mbModel, mbObject, mbModelObject) of
    (Just model, Just obj, Just modelObj) -> do
      let tableName = getModelname model 7 -- 7 beacuse "Options" is appended to the model name
      if shouldPushToDbOnly tableName _dontEnableForKafka || not isPushToKafka'
        then runUpdateQuery updateDataEntries (mbModel, mbObject, mbMappings, mbModelObject) tableName
        else do
          let updateObject = getObjectForKafka modelObj mbMappings
          res <- EL.runIO $ createInKafka _kafkaConnection updateObject streamName tableName
          case res of
            Left err -> do
              EL.logError ("KAFKA UPDATE FAILED" :: Text) (err <> " for Object :: " <> show obj)
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("KAFKA UPDATE SUCCESSFUL" :: Text) (" Update successful for object :: " <> show obj)
              runUpdateQuery updateDataEntries (mbModel, mbObject, mbMappings, mbModelObject) tableName
    _ -> do
      EL.logError ("UPDATE FAILED" :: Text) ("Invalid streamData or Extraction of data from redis stream failed :: " <> TE.decodeUtf8 streamData)
      return $ Left entryId

runUpdateQuery :: (EL.KVDBStreamEntryID, ByteString) -> (Maybe Text, Maybe Array, Maybe Object, Maybe Object) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runUpdateQuery updateDataEntries updateData tableName = do
  Env {..} <- ask
  let (entryId, byteString) = updateDataEntries
      (mbModel, mbObject, mbMappings, _) = updateData
  EL.logDebug ("BYTE STRING" :: Text) (show byteString)
  if shouldPushToKafkaOnly tableName _dontEnableDbTables
    then return $ Right entryId
    else do
      let updateQuery = getUpdateQueryForTable (mbModel, mbObject, mbMappings) tableName
      case updateQuery of
        Just query -> do
          result <- EL.runIO $ try $ executeQuery _pgConnection (Query $ TE.encodeUtf8 query)
          case result of
            Left (QueryError errorMsg) -> do
              EL.logError ("QUERY UPDATE FAILED" :: Text) (errorMsg <> " for query :: " <> query)
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("QUERY UPDATE SUCCESSFUL" :: Text) (" Update successful for query :: " <> query <> " with streamData :: " <> TE.decodeUtf8 byteString)
              return $ Right entryId
        Nothing -> do
          EL.logError ("No query generated for streamData: " :: Text) (TE.decodeUtf8 byteString)
          return $ Left entryId

-- This function is used to extract the model name, object and mappings from the stream data
getDataFromByteStringForUpdate :: ByteString -> (Maybe T.Text, Maybe A.Array, Maybe A.Object, Maybe A.Object)
getDataFromByteStringForUpdate dbCommandByteString = do
  case A.decode $ LBS.fromStrict dbCommandByteString of
    Just (A.Object o) -> case HM.lookup "contents" o of
      Just (A.Array a) -> case V.last a of
        (A.Object obj) -> (lookupText "tag" obj, lookupArray "contents" obj, lookupObject "mappings" o, lookupObject "updatedModel" o)
        _ -> (Nothing, Nothing, Nothing, Nothing)
      _ -> (Nothing, Nothing, Nothing, Nothing)
    _ -> (Nothing, Nothing, Nothing, Nothing)

getUpdateQueryForTable :: (Maybe Text, Maybe Array, Maybe Object) -> Text -> Maybe Text
getUpdateQueryForTable (mbModel, mbArray, mbMappings) tableName = do
  case (mbModel, mbArray) of
    (Just _, Just array') -> do
      let (setValues, [whereValues]) = getSetAndWhereClause (Just array')
          correctWhereClauseText = makeWhereCondition whereValues (fromMaybe HM.empty mbMappings)
          setQuery = makeSetConditions setValues (fromMaybe HM.empty mbMappings)
      if T.null correctWhereClauseText
        then Nothing
        else Just $ "UPDATE atlas_driver_offer_bpp." <> quote' tableName <> " SET " <> setQuery <> " WHERE " <> correctWhereClauseText <> ";"
    _ -> Nothing

makeSetConditions :: [(Text, Value)] -> A.Object -> Text
makeSetConditions setValues mbMappings = do
  let setClauseText = map (second valueToText) setValues
  let correctSetClausetext = map (\(k, v) -> (replaceMappings k mbMappings, v)) setClauseText
  T.intercalate "," (map (\(k, v) -> (quote' . textToSnakeCaseText) k <> "=" <> v) correctSetClausetext)

-- this is being used to get the set and where clause from the json array
getSetAndWhereClause :: Maybe A.Array -> ([(Text, A.Value)], [A.Value])
getSetAndWhereClause jsonArray = case toList <$> jsonArray of
  Just [firstArray, secondArray] -> do
    let firstList = extractValues firstArray
        secondList = extractValues secondArray
        setValues = mapMaybe extractBothValues firstList
        whereValues = mapMaybe extractBothValues secondList
        whereConditions = map snd whereValues
     in (setValues, whereConditions)
  _ -> ([], [])
