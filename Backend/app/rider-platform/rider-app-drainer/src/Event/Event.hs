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
      PeekDBCommandError -> inc (metrics </> #peek_db_command_error)
      DropDBCommandError -> inc (metrics </> #drop_db_command_error)
      ParseDBCommandError action modelObject -> inc (metrics </> #parse_db_command_error) action modelObject
      QueryExecutionFailure action model -> inc (metrics </> #query_execution_failure_error) action model
      DuplicateEntryCreate model -> inc (metrics </> #duplicate_entry_create) model
      DrainerQueryExecutes action count -> add (metrics </> #drainer_query_executes) count action
      QueryDrainLatency action latency -> observe (metrics </> #query_drain_latency) latency action
      DrainerStopStatus status -> setGauge (metrics </> #drainer_stop_status) status
      KafkaUpdateMissing -> inc (metrics </> #rider_kafka_update_missing)
      KafkaPushFailure action model -> inc (metrics </> #rider_kafka_push_failure) action model
      ProcessLatency processName latency -> observe (metrics </> #rider_process_latency) latency processName
      KvConfigDecodeFailure -> inc (metrics </> #eider_kv_config_decode_failure)
  where
    collectionDBSyncMetric =
      peek_db_command_error
        .> drop_db_command_error
        .> parse_db_command_error
        .> query_execution_failure_error
        .> duplicate_entry_create
        .> drainer_query_executes
        .> query_drain_latency
        .> drainer_stop_status
        .> rider_kafka_update_missing
        .> rider_kafka_push_failure
        .> rider_process_latency
        .> eider_kv_config_decode_failure
        .> MNil
