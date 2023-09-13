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
      PeekDBCommandError -> inc (metrics </> #driver_peek_db_command_error)
      DropDBCommandError -> inc (metrics </> #driver_drop_db_command_error)
      ParseDBCommandError action modelObject -> inc (metrics </> #driver_parse_db_command_error) action modelObject
      QueryExecutionFailure action model -> inc (metrics </> #driver_query_execution_failure_error) action model
      DuplicateEntryCreate model -> inc (metrics </> #driver_duplicate_entry_create) model
      DrainerQueryExecutes action count -> add (metrics </> #driver_drainer_query_executes) count action
      QueryDrainLatency action latency -> observe (metrics </> #driver_query_drain_latency) latency action
      DrainerStopStatus status -> setGauge (metrics </> #driver_drainer_stop_status) status
      KafkaPushFailure -> inc (metrics </> #kafka_message_push_failure)

collectionDBSyncMetric =
  driver_peek_db_command_error
    .> driver_drop_db_command_error
    .> driver_parse_db_command_error
    .> driver_query_execution_failure_error
    .> driver_duplicate_entry_create
    .> driver_drainer_query_executes
    .> driver_query_drain_latency
    .> driver_drainer_stop_status
    .> kafka_message_push_failure
    .> MNil
