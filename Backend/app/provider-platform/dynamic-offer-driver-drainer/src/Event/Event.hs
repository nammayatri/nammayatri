{-# LANGUAGE OverloadedLabels #-}

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
      PeekDBCommandError -> inc (metrics </> #driver_peek_db_command_error)
      DropDBCommandError -> inc (metrics </> #driver_drop_db_command_error)
      ParseDBCommandError action modelObject -> inc (metrics </> #driver_parse_db_command_error) action modelObject
      QueryExecutionFailure action model -> inc (metrics </> #driver_query_execution_failure_error) action model
      DuplicateEntryCreate model -> inc (metrics </> #driver_duplicate_entry_create) model
      DrainerQueryExecutes action count -> add (metrics </> #driver_drainer_query_executes) count action
      QueryDrainLatency action latency -> observe (metrics </> #driver_query_drain_latency) latency action
      DrainerStopStatus status -> setGauge (metrics </> #driver_drainer_stop_status) status
      KafkaUpdateMissing -> inc (metrics </> #driver_kafka_update_missing)
      KafkaPushFailure action model -> inc (metrics </> #driver_kafka_push_failure) action model
      ProcessLatency processName latency -> observe (metrics </> #driver_process_latency) latency processName
      KvConfigDecodeFailure -> inc (metrics </> #driver_kv_config_decode_failure)
      BatchFallbackUsed _ -> inc (metrics </> #driver_batch_fallback_used)
      BatchExecutionTime model time -> observe (metrics </> #driver_batch_execution_time) time model
      BatchEntriesProcessed model count -> add (metrics </> #driver_batch_entries_processed) (fromIntegral count) model
      SchemaVariationAlert model _ -> inc (metrics </> #driver_schema_variation_alert) model
  where
    collectionDBSyncMetric =
      driver_peek_db_command_error
        .> driver_drop_db_command_error
        .> driver_parse_db_command_error
        .> driver_query_execution_failure_error
        .> driver_duplicate_entry_create
        .> driver_drainer_query_executes
        .> driver_query_drain_latency
        .> driver_drainer_stop_status
        .> driver_kafka_update_missing
        .> driver_kafka_push_failure
        .> driver_process_latency
        .> driver_kv_config_decode_failure
        .> driver_batch_fallback_used
        .> driver_batch_execution_time
        .> driver_batch_entries_processed
        .> driver_schema_variation_alert
        .> MNil
