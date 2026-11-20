module DBSync.Create where

import Config.Env
import DBQuery.Functions
import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Text as T hiding (any, concatMap, elem, filter, map, null)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple.Types
import EulerHS.Language as EL
import EulerHS.Prelude
import qualified Kernel.Beam.Lib.Utils as KBLU
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
          tableSnake = textToSnakeCaseText tableName.getDBModel
      if shouldPushToDbOnly tableName _dontEnableForKafka || tableSnake `elem` _dropTablesForCh || tableName.getDBModel `elem` _dropTablesForCh || not isPushToKafka'
        then runCreateQuery createDataEntry createDBModel
        else do
          let chCols = columnsForTable tableSnake tableName.getDBModel _dropColumnsForCh
              rawObject = KBLU.replaceMappings (A.Object createDBModel.contentsObj) (HM.fromList . M.toList $ createDBModel.mappings.getMapping)
              createObject = filterChColumns chCols rawObject
          res <- EL.runIO $ createInKafka _kafkaProducerTools createObject streamName tableName
          case res of
            Left err -> do
              EL.logError ("KAFKA CREATE FAILED" :: Text) ("Kafka create failed for drainer : " <> err <> " for table :: " <> show tableName)
              void $ publishDBSyncMetric $ Event.KafkaPushFailure "Create" tableName.getDBModel
              _ <- runCreateQuery createDataEntry createDBModel --- it should push that entry to db what if isForcePushEnabled is true then it will get missed
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
      tableSnake = textToSnakeCaseText dbModel.getDBModel
      pgDropTable = tableSnake `elem` _dropTablesForDb || dbModel.getDBModel `elem` _dropTablesForDb
  if pgDropTable || (shouldPushToKafkaOnly dbModel _dontEnableDbTables && not dbCreateObject.forceDrainToDB)
    then return $ Right entryId
    else do
      let pgCols = columnsForTable tableSnake dbModel.getDBModel _dropColumnsForDb
          filteredObj = if null pgCols then dbCreateObject else (dbCreateObject :: DBCreateObject) {contents = filterInsertContents pgCols (dbCreateObject.contents) (dbCreateObject.mappings)}
          DBCreateObjectContent filteredTerms = filteredObj.contents
          insertQuery = generateInsertForTable filteredObj
      if not (null pgCols) && null filteredTerms
        then return $ Right entryId
        else case insertQuery of
          Just query -> do
            result <- EL.runIO $ try $ executeQueryUsingConnectionPool _connectionPool (Query $ TE.encodeUtf8 query)
            case result of
              Left (QueryError errorMsg) -> do
                EL.logError ("QUERY INSERT FAILED" :: Text) ("(ENTRY ID :: " <> show entryId <> ") => " <> errorMsg <> " for query :: " <> query)
                EL.logError ("QUERY INSERT FAILED: BYTE STRING" :: Text) (show byteString)
                EL.logError ("QUERY INSERT FAILED: DB OBJECT" :: Text) (show dbCreateObject)
                void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" dbModel.getDBModel
                return $ Left entryId
              Right _ -> do
                EL.logDebug ("QUERY INSERT SUCCESSFUL" :: Text) (" Insert successful for query :: " <> query <> " with streamData :: " <> TE.decodeUtf8 byteString)
                setDrainerTtl dbCreateObject.dbModel dbCreateObject.primaryKey
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

-- Extract column names to filter for the given table.
-- Accepts both snake_case and camelCase table prefix (e.g. "driver_location.col" or "DriverLocation.col").
-- The column part is matched both as-written and normalised to snake_case, so a config entry in either
-- case (e.g. "driver_location.some_field" or "driver_location.someField") is handled correctly, because
-- replaceMappings normalises identifiers to snake_case before comparison.
columnsForTable :: Text -> Text -> [Text] -> [Text]
columnsForTable tableSnake tableCamel = concatMap $ \tc ->
  case T.breakOn "." tc of
    (tbl, dotCol)
      | (tbl == tableSnake || tbl == tableCamel) && dotCol /= mempty ->
        let col = T.drop 1 dotCol in [col, textToSnakeCaseText col]
    _ -> []

-- Filter TermWraps to exclude configured columns before INSERT.
filterInsertContents :: [Text] -> DBCreateObjectContent -> Mapping -> DBCreateObjectContent
filterInsertContents [] c _ = c
filterInsertContents cols (DBCreateObjectContent tws) mp =
  DBCreateObjectContent $ filter (\(TermWrap col _) -> replaceMappings col mp `notElem` cols) tws

-- Filter keys from the Kafka/CH JSON payload.
filterChColumns :: [Text] -> A.Value -> A.Value
filterChColumns [] v = v
filterChColumns cols v =
  case A.fromJSON @(M.Map Text A.Value) v of
    A.Success m -> A.toJSON $ M.filterWithKey (\k _ -> k `notElem` cols) m
    A.Error _ -> v
