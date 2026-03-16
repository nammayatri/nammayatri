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

driver_kv_config_decode_failure :: PromRep 'Counter "driver_kv_config_decode_failure" '[]
driver_kv_config_decode_failure =
  counter #driver_kv_config_decode_failure
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

driver_batch_fallback_used :: PromRep 'Counter "driver_batch_fallback_used" '[]
driver_batch_fallback_used =
  counter #driver_batch_fallback_used
    .& build

driver_batch_execution_time :: PromRep 'Histogram "driver_batch_execution_time" '[ '("model", Text)]
driver_batch_execution_time =
  histogram #driver_batch_execution_time
    .& lbl @"model" @Text
    .& build

driver_batch_entries_processed :: PromRep 'Counter "driver_batch_entries_processed" '[ '("model", Text)]
driver_batch_entries_processed =
  counter #driver_batch_entries_processed
    .& lbl @"model" @Text
    .& build

driver_schema_variation_alert :: PromRep 'Counter "driver_schema_variation_alert" '[ '("model", Text)]
driver_schema_variation_alert =
  counter #driver_schema_variation_alert
    .& lbl @"model" @Text
    .& build

driver_drainer_batch_size :: PromRep 'Histogram "driver_drainer_batch_size" '[]
driver_drainer_batch_size =
  histogram #driver_drainer_batch_size
    .& build

driver_drainer_errors_total :: PromRep 'Counter "driver_drainer_errors_total" '[]
driver_drainer_errors_total =
  counter #driver_drainer_errors_total
    .& build

driver_drainer_lag_seconds :: PromRep 'Gauge "driver_drainer_lag_seconds" '[]
driver_drainer_lag_seconds =
  gauge #driver_drainer_lag_seconds
    .& build

driver_drainer_items_pending :: PromRep 'Gauge "driver_drainer_items_pending" '[]
driver_drainer_items_pending =
  gauge #driver_drainer_items_pending
    .& build

driver_dlq_item_added :: PromRep 'Counter "driver_dlq_item_added" '[ '("action", Text)]
driver_dlq_item_added =
  counter #driver_dlq_item_added
    .& lbl @"action" @Text
    .& build

driver_backpressure_activated :: PromRep 'Gauge "driver_backpressure_activated" '[]
driver_backpressure_activated =
  gauge #driver_backpressure_activated
    .& build

driver_batch_retry_attempt :: PromRep 'Counter "driver_batch_retry_attempt" '[ '("action", Text)]
driver_batch_retry_attempt =
  counter #driver_batch_retry_attempt
    .& lbl @"action" @Text
    .& build
