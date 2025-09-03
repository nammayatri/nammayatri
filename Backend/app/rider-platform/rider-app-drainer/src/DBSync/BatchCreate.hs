module DBSync.BatchCreate where

import Config.Env (getBatchCreateEnabled, getInsertBatchSize, isPushToKafka)
import qualified DBQuery.Functions as DBQ
import DBQuery.Types
import DBSync.Create (runCreate)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple.Types
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import GHC.Float (int2Double)
import Kafka.Producer as Producer
import Kernel.Beam.Lib.Utils as KBLU
import Types.DBSync
import Types.Event as Event
import Utils.Utils

-- | Lightweight logging helpers to avoid JSON overhead
logTableInfo :: Text -> DBModel -> Int -> Int -> Flow ()
logTableInfo prefix tableName sigCount totalEntries =
  EL.logInfo prefix $ tableName.getDBModel <> "|sigs:" <> show sigCount <> "|entries:" <> show totalEntries

logSignatureInfo :: Text -> DBModel -> [Text] -> Int -> Flow ()
logSignatureInfo prefix tableName columns entryCount =
  EL.logInfo prefix $ tableName.getDBModel <> "|cols:" <> show (length columns) <> "|entries:" <> show entryCount

logSplitInfo :: Text -> Int -> Int -> Flow ()
logSplitInfo prefix forcedCount batchableCount =
  EL.logInfo prefix $ "forced:" <> show forcedCount <> "|batchable:" <> show batchableCount

logBatchResults :: Text -> Int -> Int -> Int -> Int -> Flow ()
logBatchResults prefix totalSucc totalFail indivSucc batchSucc =
  EL.logInfo prefix $ "success:" <> show totalSucc <> "|fail:" <> show totalFail <> "|indiv:" <> show indivSucc <> "|batch:" <> show batchSucc

-- | Single-pass statistics calculation
calculateTableStats :: Map ColumnSignature [ParsedCreateEntry] -> (Int, Int)
calculateTableStats =
  M.foldl'
    ( \(sigCount, entryCount) entries ->
        (sigCount + 1, entryCount + length entries)
    )
    (0, 0)

-- | Column signature for grouping compatible inserts
data ColumnSignature = ColumnSignature
  { tableName :: DBModel,
    columnNames :: [Text], -- Ordered list of column names
    columnCount :: Int
  }
  deriving (Eq, Ord, Show)

-- | Enhanced parsed entry with column signature
data ParsedCreateEntry = ParsedCreateEntry
  { entryId :: EL.KVDBStreamEntryID,
    createObject :: DBCreateObject,
    originalBytes :: ByteString,
    columnSignature :: ColumnSignature
  }

-- | Generate column signature from DBCreateObject
generateColumnSignature :: DBCreateObject -> ColumnSignature
generateColumnSignature createObj =
  let DBCreateObjectContent termWraps = createObj.contents
      columns = map (\(TermWrap column _) -> DBQ.replaceMappings column createObj.mappings) termWraps
      sortedColumns = List.sort columns -- Sort for consistent comparison
   in ColumnSignature
        { tableName = createObj.dbModel,
          columnNames = sortedColumns,
          columnCount = length sortedColumns
        }

-- | Parse create entry with signature generation
parseCreateEntry :: (EL.KVDBStreamEntryID, ByteString) -> Flow (Either Text ParsedCreateEntry)
parseCreateEntry (entryId, streamData) = do
  case A.eitherDecode @DBCreateObject . LBS.fromStrict $ streamData of
    Right createObject -> do
      let signature = generateColumnSignature createObject
      pure $
        Right $
          ParsedCreateEntry
            { entryId = entryId,
              createObject = createObject,
              originalBytes = streamData,
              columnSignature = signature
            }
    Left err -> do
      let errorMsg = "Parse failed for entry " <> show entryId <> ": " <> T.pack (show err)
      EL.logError ("PARSE_CREATE_ENTRY_FAILED" :: Text) errorMsg
      pure $ Left errorMsg

-- | Main batched create function to replace executeInSequence
executeBatchedCreate :: Text -> [(EL.KVDBStreamEntryID, ByteString)] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
executeBatchedCreate dbStreamKey createEntries = do
  EL.logInfo ("STARTING_BATCHED_CREATE" :: Text) ("Total entries: " <> show (length createEntries))

  -- Step 1: Parse all entries and check for failures
  parseResults <- mapM parseCreateEntry createEntries
  let (parseErrors, parsedEntries) = partitionEithers parseResults
      totalEntries = length createEntries
      parsedCount = length parsedEntries
      failedParseCount = length parseErrors

  -- Check if any parsing failed
  if failedParseCount > 0
    then do
      EL.logError ("BATCH_CREATE_PARSE_FAILURES" :: Text) $
        "total:" <> show totalEntries <> "|parsed:" <> show parsedCount <> "|failed:" <> show failedParseCount

      -- Stop the drainer due to parsing failures
      stopDrainer

      -- Return all entries as failures
      let allEntryIds = map fst createEntries
      pure ([], allEntryIds)
    else do
      -- Step 2: Separate by forceDrainToDB flag
      let (forceIndividual, batchable) = List.partition (\entry -> entry.createObject.forceDrainToDB) parsedEntries

      logSplitInfo "PROCESSING_SPLIT" (length forceIndividual) (length batchable)

      -- Step 3: Process both groups
      individualResults <- processIndividualEntries dbStreamKey forceIndividual
      batchResults <- executeBatchableEntries dbStreamKey batchable

      -- Step 4: Combine results
      let (successes1, failures1) = individualResults
          (successes2, failures2) = batchResults

      let totalSuccesses = successes1 ++ successes2
          totalFailures = failures1 ++ failures2

      logBatchResults "BATCHED_CREATE_COMPLETE" (length totalSuccesses) (length totalFailures) (length successes1) (length successes2)

      pure (totalSuccesses, totalFailures)

-- | Convert ParsedCreateEntry back to the format expected by runCreate
parsedEntryToCreateEntry :: ParsedCreateEntry -> (EL.KVDBStreamEntryID, ByteString)
parsedEntryToCreateEntry entry = (entry.entryId, entry.originalBytes)

-- | Push successful batch entries to Kafka using exact same logic as runCreate
pushSuccessfulEntriesToKafka :: Text -> [ParsedCreateEntry] -> [EL.KVDBStreamEntryID] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
pushSuccessfulEntriesToKafka streamName entries successfulIds = do
  Env {..} <- ask
  isPushToKafka' <- EL.runIO isPushToKafka

  -- Filter entries to only those that were successfully inserted
  let successfulEntries = filter (\entry -> entry.entryId `elem` successfulIds) entries

  if null successfulEntries
    then pure (successfulIds, [])
    else do
      EL.logInfo ("PUSHING_TO_KAFKA_AFTER_BATCH" :: Text) ("Count: " <> show (length successfulEntries))

      results <- mapM (pushSingleEntryToKafka streamName isPushToKafka' _kafkaConnection _dontEnableForKafka) successfulEntries
      let (kafkaSuccesses, kafkaFailures) = partitionEithers results

      EL.logInfo ("KAFKA_PUSH_RESULTS" :: Text) $
        "total:" <> show (length successfulEntries) <> "|success:" <> show (length kafkaSuccesses) <> "|fail:" <> show (length kafkaFailures)

      pure (kafkaSuccesses, kafkaFailures)

-- | Push a single entry to Kafka using exact same logic as runCreate
pushSingleEntryToKafka :: Text -> Bool -> Producer.KafkaProducer -> [Text] -> ParsedCreateEntry -> Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
pushSingleEntryToKafka streamName isPushToKafka' kafkaConnection dontEnableForKafka entry = do
  let createDBModel = entry.createObject
      tableName = createDBModel.dbModel
      entryId = entry.entryId

  -- Use exact same logic as runCreate for deciding whether to push to Kafka
  if shouldPushToDbOnly tableName dontEnableForKafka || not isPushToKafka'
    then return $ Right entryId
    else do
      -- Use exact same object preparation logic as runCreate
      let createObject = KBLU.replaceMappings (A.Object createDBModel.contentsObj) (HM.fromList . M.toList $ createDBModel.mappings.getMapping)
      res <- EL.runIO $ createInKafka kafkaConnection createObject streamName tableName
      case res of
        Left err -> do
          EL.logError ("KAFKA CREATE FAILED" :: Text) ("Kafka create failed for drainer : " <> err <> " for table :: " <> show tableName)
          void $ publishDBSyncMetric $ Event.KafkaPushFailure "BatchCreate" tableName.getDBModel
          return $ Left entryId
        Right _ -> do
          EL.logInfo ("KAFKA CREATE SUCCESSFUL" :: Text) (" Create successful for object :: " <> show createDBModel.contents)
          return $ Right entryId

-- | Handle forceDrainToDB = True entries (individual processing) using existing runCreate
processIndividualEntries :: Text -> [ParsedCreateEntry] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
processIndividualEntries dbStreamKey entries = do
  if null entries
    then pure ([], [])
    else do
      let createEntries = map parsedEntryToCreateEntry entries

      -- Use existing runCreate with executeInSequence
      (successes, failures) <- executeInSequence runCreate ([], []) dbStreamKey createEntries

      void $ publishDBSyncMetric $ Event.DrainerQueryExecutes "IndividualCreate" (fromIntegral $ length successes)
      pure (successes, failures)

-- | Handle forceDrainToDB = False entries with column-aware batching
executeBatchableEntries :: Text -> [ParsedCreateEntry] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
executeBatchableEntries dbStreamKey entries = do
  if null entries
    then pure ([], [])
    else do
      EL.logInfo ("PROCESSING_BATCHABLE_ENTRIES" :: Text) ("Count: " <> show (length entries))

      -- Step 1: Group by table name
      let groupedByTable = groupByTable entries
          tableCount = M.size groupedByTable
      EL.logInfo ("TABLE_GROUPING" :: Text) $ "tables:" <> show tableCount

      -- Step 2: Within each table, group by column signature
      let groupedBySignature = M.map groupByColumnSignature groupedByTable

      -- Step 3: Process each signature group as a batch (shouldPushToKafkaOnly check is done per table)
      results <- mapM (processTableSignatureGroups dbStreamKey) (M.toList groupedBySignature)
      let flatResults = concat results
          (successes, failures) = unzip flatResults

      pure (concat successes, concat failures)

-- | Group entries by table
groupByTable :: [ParsedCreateEntry] -> Map DBModel [ParsedCreateEntry]
groupByTable = M.fromListWith (++) . map (\entry -> (entry.createObject.dbModel, [entry]))

-- | Group entries by column signature within a table
groupByColumnSignature :: [ParsedCreateEntry] -> Map ColumnSignature [ParsedCreateEntry]
groupByColumnSignature = M.fromListWith (++) . map (\entry -> (entry.columnSignature, [entry]))

-- | Process all signature groups for a table
processTableSignatureGroups :: Text -> (DBModel, Map ColumnSignature [ParsedCreateEntry]) -> Flow [([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])]
processTableSignatureGroups dbStreamKey (tableName, signatureGroups) = do
  Env {_dontEnableDbTables} <- ask
  let (signatureCount, totalEntries) = calculateTableStats signatureGroups

  logTableInfo ("PROCESSING_TABLE_SIGNATURES" :: Text) tableName signatureCount totalEntries

  -- Check if this table should be Kafka-only (single check for entire table)
  if shouldPushToKafkaOnly tableName _dontEnableDbTables
    then do
      -- Handle Kafka-only table (all entries here already have forceDrainToDB=false)
      EL.logInfo ("KAFKA_ONLY_TABLE" :: Text) $ tableName.getDBModel <> "|entries:" <> show totalEntries

      -- All entries skip DB and go to Kafka only
      let allEntries = concat $ M.elems signatureGroups
          kafkaIds = map (.entryId) allEntries
      kafkaResults <- pushSuccessfulEntriesToKafka dbStreamKey allEntries kafkaIds

      pure [kafkaResults]
    else -- Normal DB processing for this table
      mapM (processSignatureGroup dbStreamKey tableName) (M.toList signatureGroups)

-- | Process a single signature group
processSignatureGroup :: Text -> DBModel -> (ColumnSignature, [ParsedCreateEntry]) -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
processSignatureGroup dbStreamKey tableName (signature, entries) = do
  let entryCount = length entries
      columns = signature.columnNames
  logSignatureInfo ("PROCESSING_SIGNATURE_GROUP" :: Text) tableName columns entryCount
  canBatch <- shouldBatchSignature entryCount
  if canBatch
    then executeBatchForSignature dbStreamKey signature entries
    else executeIndividuallyForSignature dbStreamKey entries

-- | Determine if a signature group should be batched
shouldBatchSignature :: Int -> Flow Bool
shouldBatchSignature entryCount = do
  -- Minimum entries required for batching
  let minBatchSize = 2

  -- Don't batch if too few entries
  if entryCount < minBatchSize
    then pure False -- Remove debug log from hot path
    else EL.runIO getBatchCreateEnabled -- Simplified: just return the global setting

-- | Execute individual processing for small signature groups
executeIndividuallyForSignature :: Text -> [ParsedCreateEntry] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
executeIndividuallyForSignature dbStreamKey entries = do
  EL.logInfo ("EXECUTING_INDIVIDUALLY_FOR_SIGNATURE" :: Text) ("Count: " <> show (length entries))
  processIndividualEntries dbStreamKey entries

-- | Execute batch for a specific signature
executeBatchForSignature :: Text -> ColumnSignature -> [ParsedCreateEntry] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
executeBatchForSignature _dbStreamKey signature entries = do
  let createObjects = map (.createObject) entries
      entryIds = map (.entryId) entries
      batchSize = length entries

  maxBatchSize <- getGlobalBatchSize
  if batchSize <= maxBatchSize
    then executeSingleBatch signature entries entryIds createObjects
    else do
      EL.logInfo ("SPLITTING_LARGE_BATCH" :: Text) $ signature.tableName.getDBModel <> "|size:" <> show batchSize <> "|max:" <> show maxBatchSize

      let batches = splitIntoBatches maxBatchSize entries
      results <- mapM (\batch -> executeSingleBatch signature batch (map (.entryId) batch) (map (.createObject) batch)) batches
      let (successes, failures) = unzip results
      pure (concat successes, concat failures)
  where
    executeSingleBatch sig batchEntries ids objects = do
      case generateBulkInsertForSignature sig objects of
        Nothing -> do
          EL.logError ("BATCH_QUERY_GENERATION_FAILED" :: Text) (show sig)
          executeIndividuallyForSignature _dbStreamKey batchEntries
        Just bulkQuery -> do
          -- Step 1: Push to Kafka FIRST (matching individual Create order)
          (kafkaSuccesses, kafkaFailures) <- pushSuccessfulEntriesToKafka _dbStreamKey batchEntries ids

          -- If any Kafka pushes failed, fall back to individual processing
          if not (null kafkaFailures)
            then do
              EL.logInfo ("KAFKA_FAILURES_FALLBACK" :: Text) ("Kafka failures: " <> show (length kafkaFailures) <> ", falling back to individual")
              executeIndividuallyForSignature _dbStreamKey batchEntries
            else do
              -- Step 2: Push to DB after Kafka success (matching individual Create order)
              Env {_connectionPool} <- ask
              startTime <- EL.getCurrentDateInMillis
              result <- EL.runIO $ try $ DBQ.executeQueryUsingConnectionPool _connectionPool (Query $ TE.encodeUtf8 bulkQuery)
              endTime <- EL.getCurrentDateInMillis
              let executionTime = int2Double (endTime - startTime)
              case result of
                Left (QueryError errorMsg) -> do
                  EL.logError ("BATCH_INSERT_FAILED" :: Text) $
                    sig.tableName.getDBModel <> "|entries:" <> show (length batchEntries) <> "|error:" <> T.take 100 errorMsg

                  void $ publishDBSyncMetric $ Event.QueryExecutionFailure "BatchCreate" sig.tableName.getDBModel
                  EL.logInfo ("FALLING_BACK_TO_INDIVIDUAL" :: Text) ("Batch size: " <> show (length batchEntries))
                  executeIndividuallyForSignature _dbStreamKey batchEntries
                Right _ -> do
                  EL.logInfo ("BATCH_INSERT_SUCCESS" :: Text) $
                    sig.tableName.getDBModel <> "|entries:" <> show (length batchEntries) <> "|cols:" <> show (length sig.columnNames)
                  void $ publishDBSyncMetric $ Event.DrainerQueryExecutes "BatchCreate" (fromIntegral $ length batchEntries)
                  void $ publishDBSyncMetric $ Event.BatchExecutionTime sig.tableName.getDBModel executionTime

                  EL.logInfo ("BATCH_WITH_KAFKA_COMPLETE" :: Text) $
                    sig.tableName.getDBModel <> "|kafka:" <> show (length kafkaSuccesses) <> "|db:" <> show (length ids) <> "|final:" <> show (length kafkaSuccesses)

                  pure (kafkaSuccesses, [])

-- | Get global batch size from environment variables
getGlobalBatchSize :: Flow Int
getGlobalBatchSize = EL.runIO getInsertBatchSize

-- | Split entries into batches of specified size
splitIntoBatches :: Int -> [a] -> [[a]]
splitIntoBatches _ [] = []
splitIntoBatches size items =
  let (batch, rest) = splitAt size items
   in batch : splitIntoBatches size rest

generateBulkInsertForSignature :: ColumnSignature -> [DBCreateObject] -> Maybe Text
generateBulkInsertForSignature signature createObjects = do
  guard (not $ null createObjects)
  let allObjectsMatch = all (\obj -> generateColumnSignature obj == signature) createObjects
  guard allObjectsMatch

  let schema = SchemaName $ T.pack DBQ.currentSchemaName
      tableName = DBQ.quote' (DBQ.textToSnakeCaseText signature.tableName.getDBModel)
      columnNamesText = T.intercalate ", " $ map DBQ.quote' signature.columnNames
  valueRows <- mapM (generateValueRowForSignature signature) createObjects
  let valuesText = T.intercalate ", " valueRows

  Just $
    "INSERT INTO " <> schema.getSchemaName <> "." <> tableName
      <> " ("
      <> columnNamesText
      <> ") VALUES "
      <> valuesText
      <> " ON CONFLICT DO NOTHING;"

-- | Generate a single value row for the signature
generateValueRowForSignature :: ColumnSignature -> DBCreateObject -> Maybe Text
generateValueRowForSignature signature createObj = do
  let DBCreateObjectContent termWraps = createObj.contents
      -- Create a map for quick lookup
      valueMap = M.fromList $ map (\(TermWrap column value) -> (DBQ.replaceMappings column createObj.mappings, value)) termWraps
      -- Extract values in the signature's column order
      values = mapM (`M.lookup` valueMap) signature.columnNames

  case values of
    Just vals -> Just $ "(" <> T.intercalate ", " (map DBQ.valueToText vals) <> ")"
    Nothing -> Nothing -- Column mismatch, shouldn't happen due to validation
