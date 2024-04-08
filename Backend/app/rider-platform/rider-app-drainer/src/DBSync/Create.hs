module DBSync.Create where

import Config.Env
import DBQuery.Functions
import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text as T hiding (any, map, null)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple.Types
import EulerHS.Language as EL
import EulerHS.Prelude
import Kernel.Beam.Lib.Utils as KBLU
import Text.Casing (pascal)
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
      if shouldPushToDbOnly tableName _dontEnableForKafka || not isPushToKafka'
        then runCreateQuery createDataEntry createDBModel
        else do
          let createObject = KBLU.replaceMappings (A.Object createDBModel.contentsObj) (HM.fromList . M.toList $ createDBModel.mappings.getMapping)
          res <- EL.runIO $ createInKafka _kafkaConnection createObject streamName tableName
          case res of
            Left err -> do
              EL.logError ("KAFKA CREATE FAILED" :: Text) ("Kafka create failed for drainer : " <> err <> " for table :: " <> show tableName)
              void $ publishDBSyncMetric $ Event.KafkaPushFailure "Create" tableName.getDBModel
              return $ Left entryId
            Right _ -> do
              EL.logInfo ("KAFKA CREATE SUCCESSFUL" :: Text) (" Create successful for object :: " <> show createDBModel.contents)
              runCreateQuery createDataEntry createDBModel
    Left err -> do
      EL.logError ("CREATE FAILED" :: Text) ("Invalid streamData or Extraction of data from redis stream failed :: " <> TE.decodeUtf8 streamData <> "; error :: " <> show err)
      return $ Left entryId

-- | Run a create query for a single entry in the stream
runCreateQuery :: (EL.KVDBStreamEntryID, ByteString) -> DBCreateObject -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runCreateQuery createDataEntry dbCreateObject' = do
  Env {..} <- ask
  let (entryId, byteString) = createDataEntry
      dbModel = dbCreateObject'.dbModel
  if shouldPushToKafkaOnly dbModel _dontEnableDbTables
    then return $ Right entryId
    else do
      -- Temporary for debug issue with huge values
      let disToPickupThreshold = 1000000.0
      let condition = invalidNumCondition (\d -> abs d > disToPickupThreshold)
      mbUpdDbCreateObject <- replaceInvalidValue byteString dbCreateObject' (DBModel "BookingCancellationReason") (Column "driverDistToPickup") condition (SqlNum (-1.0))
      let dbCreateObject = fromMaybe dbCreateObject' mbUpdDbCreateObject

      let insertQuery = generateInsertForTable dbCreateObject
      case insertQuery of
        Just query -> do
          result <- EL.runIO $ try $ executeQuery _pgConnection (Query $ TE.encodeUtf8 query)
          case result of
            Left (QueryError errorMsg) -> do
              EL.logError ("QUERY INSERT FAILED" :: Text) (errorMsg <> " for query :: " <> query)
              EL.logError ("QUERY INSERT FAILED: BYTE STRING" :: Text) (show byteString)
              EL.logError ("QUERY INSERT FAILED: DB OBJECT" :: Text) (show dbCreateObject)
              void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" dbModel.getDBModel
              return $ Left entryId
            Right _ -> do
              EL.logDebug ("QUERY INSERT SUCCESSFUL" :: Text) (" Insert successful for query :: " <> query <> " with streamData :: " <> TE.decodeUtf8 byteString)
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

invalidNumCondition :: (Double -> Bool) -> (Value -> Bool)
invalidNumCondition cond (SqlNum d) = cond d
invalidNumCondition _ _ = False

-- TODO remove after debug
replaceInvalidValue :: ByteString -> DBCreateObject -> DBModel -> Column -> (Value -> Bool) -> Value -> ReaderT Env EL.Flow (Maybe DBCreateObject)
replaceInvalidValue byteString dbCreateObject checkDbModel checkColumn condition defaultVal = do
  if dbCreateObject.dbModel == checkDbModel
    then do
      let DBCreateObjectContent columns = dbCreateObject.contents
      let mbColumn = List.find (\(TermWrap column _) -> column == checkColumn) columns
      case mbColumn of
        Nothing -> pure Nothing
        Just (TermWrap _ value) ->
          if condition value
            then do
              logError ("REPLACE INVALID VALUE" :: Text) $
                "checkDbModel: "
                  <> show checkDbModel
                  <> "checkColumn: "
                  <> show checkColumn
                  <> "invalid value: "
                  <> show value
                  <> "defaultValue: "
                  <> show defaultVal
              EL.logError ("REPLACE INVALID VALUE: BYTE STRING" :: Text) (show byteString)
              EL.logError ("REPLACE INVALID VALUE: DB OBJECT" :: Text) (show dbCreateObject)
              let updValue (TermWrap column val) =
                    if column == checkColumn
                      then TermWrap column defaultVal
                      else TermWrap column val
              let updColumns = map updValue columns
              pure $ Just dbCreateObject{contents = DBCreateObjectContent updColumns}
            else pure Nothing
    else pure Nothing
