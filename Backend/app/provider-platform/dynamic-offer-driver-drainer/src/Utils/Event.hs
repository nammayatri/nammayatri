{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Utils.Event where

import Data.Text
import Euler.Events.MetricApi.MetricApi

driver_peek_db_command_error :: PromRep 'Counter "driver_peek_db_command_error" '[]
driver_peek_db_command_error =
  counter #driver_peek_db_command_error
    .& build

driver_drop_db_command_error :: PromRep 'Counter "driver_drop_db_command_error" '[]
driver_drop_db_command_error =
  counter #driver_drop_db_command_error
    .& build

driver_parse_db_command_error :: PromRep 'Counter "driver_parse_db_command_error" '[ '("action", Text), '("model_object", Text)]
driver_parse_db_command_error =
  counter #driver_parse_db_command_error
    .& lbl @"action" @Text
    .& lbl @"model_object" @Text
    .& build

driver_query_execution_failure_error :: PromRep 'Counter "driver_query_execution_failure_error" '[ '("action", Text), '("model", Text)]
driver_query_execution_failure_error =
  counter #driver_query_execution_failure_error
    .& lbl @"action" @Text
    .& lbl @"model" @Text
    .& build

driver_duplicate_entry_create :: PromRep 'Counter "driver_duplicate_entry_create" '[ '("model", Text)]
driver_duplicate_entry_create =
  counter #driver_duplicate_entry_create
    .& lbl @"model" @Text
    .& build

driver_query_drain_latency :: PromRep 'Histogram "driver_query_drain_latency" '[ '("action", Text)]
driver_query_drain_latency =
  histogram #driver_query_drain_latency
    .& lbl @"action" @Text
    .& build

driver_drainer_query_executes :: PromRep 'Counter "driver_drainer_query_executes" '[ '("action", Text)]
driver_drainer_query_executes =
  counter #driver_drainer_query_executes
    .& lbl @"action" @Text
    .& build

driver_process_latency :: PromRep 'Histogram "driver_process_latency" '[ '("process_name", Text)]
driver_process_latency =
  histogram #driver_process_latency
    .& lbl @"process_name" @Text
    .& build

driver_drainer_stop_status :: PromRep 'Gauge "driver_drainer_stop_status" '[]
driver_drainer_stop_status =
  gauge #driver_drainer_stop_status
    .& build

driver_kafka_push_failure :: PromRep 'Counter "driver_kafka_push_failure" '[ '("action", Text), '("model", Text)]
driver_kafka_push_failure =
  counter #driver_kafka_push_failure
    .& lbl @"action" @Text
    .& lbl @"model" @Text
    .& build

driver_kafka_update_missing :: PromRep 'Counter "driver_kafka_update_missing" '[]
driver_kafka_update_missing =
  counter #driver_kafka_update_missing
    .& build
