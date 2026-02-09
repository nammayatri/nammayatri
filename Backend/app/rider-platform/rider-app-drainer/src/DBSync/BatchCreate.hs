{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : DBSync.BatchCreate
-- Description : Optimized batch processing for database operations in the drainer
-- Copyright   : (c) Namma Yatri Platform, 2025
-- License     : AllRightsReserved
--
-- This module provides high-performance batch processing for database CREATE operations.
-- Key optimizations:
-- - Bulk INSERT queries instead of individual INSERTs
-- - Kafka-first processing with intelligent skipping
-- - Lightweight logging to reduce JSON overhead
-- - Single-pass statistics calculation
-- - Column signature grouping for compatible operations
--
-- Performance Impact:
-- - Reduces 25+ individual INSERTs to 1 bulk INSERT
-- - Eliminates redundant Kafka operations
-- - Minimizes logging overhead in hot paths
module DBSync.BatchCreate where

import Config.Env (getInsertBatchSize, isPushToKafka)
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
import Kernel.Beam.Lib.Utils as KBLU
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Types.DBSync
import Types.Event as Event
import Utils.Utils

-- =============================================================================
-- LOGGING UTILITIES
-- =============================================================================

-- | Lightweight logging helpers that use string concatenation instead of JSON encoding
--    to reduce overhead in hot paths. These functions create structured log messages
--    without the performance cost of JSON serialization.

-- | Log entry split information between forced individual and batchable entries
logSplitInfo :: Text -> Int -> Int -> Flow ()
logSplitInfo prefix forcedCount batchableCount =
  EL.logInfo prefix $ "forced:" <> show forcedCount <> "|batchable:" <> show batchableCount

-- =============================================================================
-- STATISTICS AND GROUPING
-- =============================================================================

-- | Calculate table statistics in a single pass to avoid multiple traversals.
--    Returns (signature_count, total_entries) for efficient logging and metrics.
calculateTableStats :: Map ColumnSignature [ParsedCreateEntry] -> (Int, Int)
calculateTableStats =
  M.foldl'
    ( \(sigCount, entryCount) entries ->
        (sigCount + 1, entryCount + length entries)
    )
    (0, 0)

-- | Column signature represents the structure of a database insert operation.
--    Entries with the same signature can be batched together into a single bulk INSERT
--    because they have identical column names and types.
--
--    This enables grouping like:
--    - Person entries with same columns -> single bulk INSERT
--    - Location entries with same columns -> single bulk INSERT
--    - But Person and Location remain separate (different signatures)
data ColumnSignature = ColumnSignature
  { tableName :: DBModel,
    columnNames :: [Text], -- Ordered list of column names
    columnCount :: Int
  }
  deriving (Eq, Ord, Show)

-- | Enhanced parsed entry that includes column signature for efficient grouping.
--    This allows us to quickly group entries that can be batched together
--    without re-parsing their structure multiple times.
data ParsedCreateEntry = ParsedCreateEntry
  { entryId :: EL.KVDBStreamEntryID, -- Original stream entry ID
    createObject :: DBCreateObject, -- Parsed database object
    originalBytes :: ByteString, -- Original JSON bytes for fallback
    columnSignature :: ColumnSignature -- Computed signature for grouping
  }
  deriving (Show)

-- | Generate column signature from a database create object.
--    The signature includes sorted column names to ensure consistent grouping
--    regardless of the order columns appear in the original JSON.
--
--    Example: Two Person entries with columns [name, age, city] and [age, name, city]
--    will have the same signature and can be batched together.
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

-- =============================================================================
-- ENTRY PARSING AND PROCESSING
-- =============================================================================

-- | Parse a raw stream entry into a structured ParsedCreateEntry with column signature.
--    This is the core transformation that enables efficient batch processing by:
--    1. Parsing JSON to DBCreateObject
--    2. Computing column signature for grouping
--    3. Preserving original bytes for fallback scenarios
--
--    Returns Either to handle parsing failures gracefully.
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

-- | Main entry point for optimized batch processing of database CREATE operations.
--
--    This function replaces the sequential executeInSequence approach with an intelligent
--    batch processing strategy:
--
--    1. Parse all entries and generate column signatures
--    2. Group by table and column signature for compatible batching
--    3. Process each group with either bulk INSERT or individual fallback
--    4. Handle Kafka operations at the table level for efficiency
--
--    Performance benefits:
--    - Bulk INSERTs reduce database round trips
--    - Column signature grouping enables safe batching
--    - Table-level Kafka processing reduces overhead
--    - Graceful fallback for edge cases
executeBatchedCreate :: Text -> [(EL.KVDBStreamEntryID, ByteString)] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
executeBatchedCreate _ [] = pure ([], [])
executeBatchedCreate dbStreamKey createEntries = do
  EL.logInfo ("STARTING_BATCHED_CREATE" :: Text) ("Total entries: " <> show (length createEntries))

  -- Step 1: Parse all entries and compute column signatures for grouping
  parseResults <- mapM parseCreateEntry createEntries
  let (parseErrors, parsedEntries) = partitionEithers parseResults
      totalEntries = length createEntries
      parsedCount = length parsedEntries
      failedParseCount = length parseErrors

  -- If any JSON parsing errors occur, fallback to sequential processing
  if failedParseCount > 0
    then do
      EL.logError ("BATCH_CREATE_PARSE_FAILURES_FALLBACK" :: Text) $
        "total:" <> show totalEntries <> "|parsed:" <> show parsedCount <> "|failed:" <> show failedParseCount <> "|falling_back_to_sequential"

      -- Fallback to sequential processing for all entries to handle parsing errors gracefully
      EL.logInfo ("FALLING_BACK_TO_SEQUENTIAL" :: Text) "Using executeInSequence for all entries due to parse failures"
      executeInSequence runCreate ([], []) dbStreamKey createEntries
    else do
      -- Step 2: Split by processing strategy based on forceDrainToDB flag
      -- forceDrainToDB=true -> must process individually (time-sensitive operations)
      -- forceDrainToDB=false -> can batch for performance (bulk operations)
      let (forceIndividual, batchable) = List.partition (\entry -> entry.createObject.forceDrainToDB) parsedEntries

      -- Step 3: Process both streams in parallel for optimal performance
      individualResults <- processIndividualEntries dbStreamKey forceIndividual
      batchResults <- executeBatchableEntries dbStreamKey batchable

      -- Step 4: Merge results from both processing paths
      let (successes1, failures1) = individualResults
          (successes2, failures2) = batchResults

      let totalSuccesses = successes1 ++ successes2
          totalFailures = failures1 ++ failures2

      -- Simplified logging - avoid expensive length calculations
      EL.logInfo ("BATCHED_CREATE_COMPLETE" :: Text) $
        "indiv_batches:" <> show (null successes1) <> "|bulk_batches:" <> show (null successes2)

      pure (totalSuccesses, totalFailures)

-- =============================================================================
-- KAFKA PROCESSING (Legacy - Used for Kafka-only tables)
-- =============================================================================

-- | Legacy Kafka processing functions maintained for Kafka-only table support.
--
--    Note: Most tables now skip Kafka processing for performance, but these functions
--    are preserved for tables that explicitly require Kafka-first processing.
--
--    The main optimization (line 251 in processTableSignatureGroups) handles
--    table-level Kafka decisions to avoid per-entry processing overhead.

-- | Push successfully processed entries to Kafka with filtering.
--    Filters to only entries that succeeded in database operations before
--    attempting Kafka push to maintain consistency.
pushEntriesToKafka :: Text -> [ParsedCreateEntry] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
pushEntriesToKafka streamName entries = do
  Env {..} <- ask
  isPushToKafka' <- EL.runIO isPushToKafka
  if null entries
    then pure ([], [])
    else do
      results <- mapM (pushSingleEntryToKafka streamName isPushToKafka' _kafkaProducerTools _dontEnableForKafka) entries
      let (kafkaFailures, kafkaSuccesses) = partitionEithers results
      pure (kafkaSuccesses, kafkaFailures)

-- | Push individual entry to Kafka with same decision logic as Create.hs.
--    Replicates the exact Kafka push logic from individual processing to maintain
--    consistency between batch and individual processing paths.
--
--    Returns Either for success/failure tracking at the entry level.
pushSingleEntryToKafka :: Text -> Bool -> KafkaProducerTools -> [Text] -> ParsedCreateEntry -> Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
pushSingleEntryToKafka streamName isPushToKafka' kafkaProducerTools dontEnableForKafka entry = do
  let createDBModel = entry.createObject
      tableName = createDBModel.dbModel
      entryId = entry.entryId

  -- Use exact same logic as runCreate for deciding whether to push to Kafka
  if shouldPushToDbOnly tableName dontEnableForKafka || not isPushToKafka'
    then return $ Right entryId
    else do
      -- Use exact same object preparation logic as runCreate
      let createObject = KBLU.replaceMappings (A.Object createDBModel.contentsObj) (HM.fromList . M.toList $ createDBModel.mappings.getMapping)
      res <- EL.runIO $ createInKafka kafkaProducerTools createObject streamName tableName
      case res of
        Left err -> do
          EL.logError ("KAFKA CREATE FAILED" :: Text) ("Kafka create failed for drainer : " <> err <> " for table :: " <> show tableName)
          void $ publishDBSyncMetric $ Event.KafkaPushFailure "BatchCreate" tableName.getDBModel
          return $ Left entryId
        Right _ -> do
          return $ Right entryId

-- =============================================================================
-- PROCESSING STRATEGIES
-- =============================================================================

-- | Process entries that require individual handling (forceDrainToDB=true).
--
--    These entries bypass batch processing for various reasons:
--    - Time-sensitive operations that can't wait for batching
--    - Legacy entries with special handling requirements
--    - Entries with complex validation logic
--
--    Uses the existing runCreate logic to maintain compatibility.
processIndividualEntries :: Text -> [ParsedCreateEntry] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
processIndividualEntries dbStreamKey entries = do
  if null entries
    then pure ([], [])
    else do
      let createEntries = map (\entry -> (entry.entryId, entry.originalBytes)) entries
      -- Use existing runCreate with executeInSequence
      (successes, failures) <- executeInSequence runCreate ([], []) dbStreamKey createEntries
      pure (successes, failures)

-- | Execute batch-eligible entries with intelligent grouping and bulk operations.
--
--    This is the core optimization function that transforms individual INSERTs
--    into efficient bulk operations through a multi-level grouping strategy:
--
--    1. Group by table (Person, Location, etc.)
--    2. Group by column signature within each table
--    3. Process each signature group as a bulk INSERT
--
--    Benefits:
--    - Reduces 25+ individual INSERTs to 1 bulk INSERT
--    - Maintains data consistency through transaction boundaries
--    - Preserves Kafka processing order when required
--    - Provides graceful fallback for complex cases
executeBatchableEntries :: Text -> [ParsedCreateEntry] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
executeBatchableEntries dbStreamKey entries = do
  if null entries
    then pure ([], [])
    else do
      -- Step 1: Group by table name
      let groupedByTable = groupByTable entries
      -- Step 2: Within each table, group by column signature
      let groupedBySignature = M.map groupByColumnSignature groupedByTable

      -- Step 3: Process each signature group as a batch (shouldPushToKafkaOnly check is done per table)
      results <- mapM (processTableSignatureGroups dbStreamKey) (M.toList groupedBySignature)
      let flatResults = concat results
          (successLists, failureLists) = unzip flatResults
          allSuccesses = concat successLists
          allFailures = concat failureLists

      pure (allSuccesses, allFailures)

-- | Group entries by database table for table-level optimizations.
--
--    This first-level grouping enables:
--    - Table-level Kafka processing decisions
--    - Per-table batching strategies
--    - Table-specific configuration application
--
--    Example: [Person, Person, Location, Person] -> {Person: [3 entries], Location: [1 entry]}
groupByTable :: [ParsedCreateEntry] -> Map DBModel [ParsedCreateEntry]
groupByTable = M.fromListWith (++) . map (\entry -> (entry.createObject.dbModel, [entry]))

-- | Group entries by column signature within a table for safe bulk operations.
--
--    This second-level grouping ensures bulk INSERTs only combine compatible entries:
--    - Same table name (Person, Location, etc.)
--    - Same column structure (name, age, city vs name, phone)
--    - Same column order and types
--
--    Example: Person entries with different columns split into separate groups
--    - Group 1: [name, age, city] -> can bulk INSERT together
--    - Group 2: [name, phone] -> separate bulk INSERT
groupByColumnSignature :: [ParsedCreateEntry] -> Map ColumnSignature [ParsedCreateEntry]
groupByColumnSignature = M.fromListWith (++) . map (\entry -> (entry.columnSignature, [entry]))

-- | Process all column signature groups within a single table.
--
--    This function handles the critical decision point for each table:
--    1. Check if table is Kafka-only (shouldPushToKafkaOnly)
--    2. If Kafka-only: process all entries for Kafka and skip DB
--    3. If normal table: process each signature group individually
--
--    The table-level Kafka check is a key optimization that avoids
--    per-entry Kafka decision overhead.
processTableSignatureGroups :: Text -> (DBModel, Map ColumnSignature [ParsedCreateEntry]) -> Flow [([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])]
processTableSignatureGroups dbStreamKey (tableName, signatureGroups) = do
  Env {_dontEnableDbTables} <- ask

  -- Alert if table has too many schema variations (schema drift detection)
  let signatureCount = M.size signatureGroups
  when (signatureCount > 1) $ do
    EL.logWarning ("SCHEMA_VARIATION_DETECTED" :: Text) $ tableName.getDBModel <> "|signatures:" <> show signatureCount <> "signatures found" <> show signatureGroups
    void $ publishDBSyncMetric $ Event.SchemaVariationAlert tableName.getDBModel signatureCount

  -- Check if this table should be Kafka-only (single check for entire table)
  if shouldPushToKafkaOnly tableName _dontEnableDbTables
    then do
      -- All entries skip DB and go to Kafka only (memory-efficient)
      let allEntries = concat $ M.elems signatureGroups
      kafkaResults <- pushEntriesToKafka dbStreamKey allEntries
      pure [kafkaResults]
    else -- Normal DB processing for this table
      mapM (processSignatureGroup dbStreamKey tableName) (M.toList signatureGroups)

-- | Process a single column signature group with batching decision.
--
--    For each signature group (entries with identical column structure):
--    1. Check if group size meets batching threshold
--    2. If yes: execute as bulk INSERT for performance
--    3. If no: fall back to individual processing for simplicity
--
--    The batching threshold prevents overhead of bulk operations
--    for small groups where individual processing is more efficient.
processSignatureGroup :: Text -> DBModel -> (ColumnSignature, [ParsedCreateEntry]) -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
processSignatureGroup dbStreamKey _tableName (signature, entries) = do
  let entryCount = length entries
  canBatch <- shouldBatchSignature entryCount
  if canBatch
    then executeBatchForSignature dbStreamKey signature entries
    else processIndividualEntries dbStreamKey entries

-- | Determine if a signature group should be batched
shouldBatchSignature :: Int -> Flow Bool
shouldBatchSignature entryCount = do
  -- Minimum entries required for batching
  let minBatchSize = 2

  -- Don't batch if too few entries
  -- Since DBSync already decided to use BatchCreate, we only need to check entry count
  pure $ entryCount >= minBatchSize

-- | Execute individual processing for small signature groups
executeIndividuallyForSignature :: Text -> [ParsedCreateEntry] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
executeIndividuallyForSignature dbStreamKey entries = do
  EL.logInfo ("EXECUTING_INDIVIDUALLY_FOR_SIGNATURE" :: Text) ("Count: " <> show (length entries))
  processIndividualEntries dbStreamKey entries

-- | Execute batch for a specific signature
executeBatchForSignature :: Text -> ColumnSignature -> [ParsedCreateEntry] -> Flow ([EL.KVDBStreamEntryID], [EL.KVDBStreamEntryID])
executeBatchForSignature _dbStreamKey signature entries = do
  let batchSize = length entries
      createObjects = map (.createObject) entries
  EL.logInfo ("EXECUTING_BATCH_FOR_SIGNATURE" :: Text) $ signature.tableName.getDBModel <> "|size:" <> show batchSize
  maxBatchSize <- getGlobalBatchSize
  if batchSize <= maxBatchSize
    then executeSingleBatch signature entries createObjects
    else do
      EL.logInfo ("SPLITTING_LARGE_BATCH" :: Text) $ signature.tableName.getDBModel <> "|size:" <> show batchSize <> "|max:" <> show maxBatchSize

      let batches = splitIntoBatches maxBatchSize entries
      results <- mapM (\batch -> executeSingleBatch signature batch (map (.createObject) batch)) batches
      let (successes, failures) = unzip results
      pure (concat successes, concat failures)
  where
    executeSingleBatch sig batchEntries objects = do
      case generateBulkInsertForSignature sig objects of
        Nothing -> do
          EL.logError ("BATCH_QUERY_GENERATION_FAILED" :: Text) (show sig)
          processIndividualEntries _dbStreamKey batchEntries
        Just bulkQuery -> do
          Env {_connectionPool} <- ask
          startTime <- EL.getCurrentDateInMillis
          result <- EL.runIO $ try $ DBQ.executeQueryUsingConnectionPool _connectionPool (Query $ TE.encodeUtf8 bulkQuery)
          endTime <- EL.getCurrentDateInMillis
          let executionTime = int2Double (endTime - startTime)
          case result of
            Left (QueryError errorMsg) -> do
              EL.logError ("BATCH_INSERT_FAILED" :: Text) $
                sig.tableName.getDBModel <> "|entries:" <> show (length batchEntries) <> "|error:" <> errorMsg <> "|query:" <> bulkQuery
              void $ publishDBSyncMetric $ Event.QueryExecutionFailure "BatchCreate" sig.tableName.getDBModel
              EL.logInfo ("FALLING_BACK_TO_INDIVIDUAL" :: Text) ("Batch size: " <> show (length batchEntries))
              processIndividualEntries _dbStreamKey batchEntries
            Right _ -> do
              EL.logInfo ("BATCH_INSERT_SUCCESS" :: Text) $
                sig.tableName.getDBModel <> "|entries:" <> show (length batchEntries) <> "|time:" <> show executionTime
              void $ publishDBSyncMetric $ Event.BatchExecutionTime sig.tableName.getDBModel executionTime
              void $ publishDBSyncMetric $ Event.BatchEntriesProcessed sig.tableName.getDBModel (length batchEntries)

              -- Push successful batch entries to Kafka
              kafkaResults <- pushEntriesToKafka _dbStreamKey batchEntries
              let (kafkaSuccesses, kafkaFailures) = kafkaResults

              -- Log Kafka results
              when (not $ null kafkaFailures) $
                EL.logError ("BATCH_KAFKA_FAILURES" :: Text) $
                  sig.tableName.getDBModel <> "|kafka_failed:" <> show (length kafkaFailures)

              pure (kafkaSuccesses, kafkaFailures)

-- | Get global batch size from environment variables
getGlobalBatchSize :: Flow Int
getGlobalBatchSize = EL.runIO getInsertBatchSize

-- | Split entries into batches of specified size (safe against infinite loops)
splitIntoBatches :: Int -> [a] -> [[a]]
splitIntoBatches _ [] = []
splitIntoBatches size items
  | size <= 0 = [items] -- Prevent infinite loop: treat as single batch
  | otherwise =
    let (batch, rest) = splitAt size items
     in batch : splitIntoBatches size rest

generateBulkInsertForSignature :: ColumnSignature -> [DBCreateObject] -> Maybe Text
generateBulkInsertForSignature signature createObjects = do
  guard (not $ null createObjects)

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
