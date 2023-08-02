{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Event.Event where

import Euler.Events.MetricApi.MetricApi
import EulerHS.Prelude
import Types.Event
import Utils.Event

mkDBSyncMetric :: IO DBSyncCounterHandler
mkDBSyncMetric = do
  metrics <- register collectionDBSyncMetric
  pure $
    DBSyncCounterHandler $ \case
      PeekDBCommandError -> inc (metrics </> #peek_db_command_error)
      DropDBCommandError -> inc (metrics </> #drop_db_command_error)
      ParseDBCommandError action modelObject -> inc (metrics </> #parse_db_command_error) action modelObject
      QueryExecutionFailure action model -> inc (metrics </> #query_execution_failure_error) action model
      DuplicateEntryCreate model -> inc (metrics </> #duplicate_entry_create) model
      -- QueryDrainLatency action latency -> observe (metrics </> #euler_query_drain_latency) latency action
      -- DBSyncStreamLength stream streamLength -> setGauge (metrics </> #euler_db_sync_stream_length) streamLength stream
      -- QueryBatchProcessTime processTime -> observe (metrics </> #euler_query_batch_process_time) processTime
      -- QueryBatchSize batchSize -> setGauge (metrics </> #euler_query_batch_size) batchSize
      DrainerQueryExecutes action count -> add (metrics </> #drainer_query_executes) count action
      DrainerStopStatus status -> setGauge (metrics </> #drainer_stop_status) status

collectionDBSyncMetric =
  peek_db_command_error
    .> drop_db_command_error
    .> parse_db_command_error
    .> query_execution_failure_error
    .> duplicate_entry_create
    -- .> euler_query_drain_latency
    -- .> euler_db_sync_stream_length
    -- .> euler_query_batch_process_time
    -- .> euler_query_batch_size
    .> drainer_query_executes
    .> drainer_stop_status
    .> MNil
