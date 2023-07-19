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
      PeekDBCommandError -> inc (metrics </> #euler_peek_db_command_error)
      DropDBCommandError -> inc (metrics </> #euler_drop_db_command_error)
      ParseDBCommandError action modelObject -> inc (metrics </> #euler_parse_db_command_error) action modelObject
      QueryExecutionFailure action model -> inc (metrics </> #euler_query_execution_failure_error) action model
      DuplicateEntryCreate model -> inc (metrics </> #euler_duplicate_entry_create) model
      QueryDrainLatency action latency -> observe (metrics </> #euler_query_drain_latency) latency action
      DBSyncStreamLength stream streamLength -> setGauge (metrics </> #euler_db_sync_stream_length) streamLength stream
      QueryBatchProcessTime processTime -> observe (metrics </> #euler_query_batch_process_time) processTime
      QueryBatchSize batchSize -> setGauge (metrics </> #euler_query_batch_size) batchSize
      DrainerQueryExecutes action count -> add (metrics </> #euler_drainer_query_executes) count action
      DrainerStopStatus status -> setGauge (metrics </> #euler_drainer_stop_status) status

collectionDBSyncMetric =
  euler_peek_db_command_error
    .> euler_drop_db_command_error
    .> euler_parse_db_command_error
    .> euler_query_execution_failure_error
    .> euler_duplicate_entry_create
    .> euler_query_drain_latency
    .> euler_db_sync_stream_length
    .> euler_query_batch_process_time
    .> euler_query_batch_size
    .> euler_drainer_query_executes
    .> euler_drainer_stop_status
    .> MNil
