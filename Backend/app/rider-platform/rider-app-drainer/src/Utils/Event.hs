{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Utils.Event where

import Data.Text
import Euler.Events.MetricApi.MetricApi

peek_db_command_error :: PromRep 'Counter "peek_db_command_error" '[]
peek_db_command_error =
  counter #peek_db_command_error
    .& build

drop_db_command_error :: PromRep 'Counter "drop_db_command_error" '[]
drop_db_command_error =
  counter #drop_db_command_error
    .& build

parse_db_command_error :: PromRep 'Counter "parse_db_command_error" '[ '("action", Text), '("model_object", Text)]
parse_db_command_error =
  counter #parse_db_command_error
    .& lbl @"action" @Text
    .& lbl @"model_object" @Text
    .& build

query_execution_failure_error :: PromRep 'Counter "query_execution_failure_error" '[ '("action", Text), '("model", Text)]
query_execution_failure_error =
  counter #query_execution_failure_error
    .& lbl @"action" @Text
    .& lbl @"model" @Text
    .& build

duplicate_entry_create :: PromRep 'Counter "duplicate_entry_create" '[ '("model", Text)]
duplicate_entry_create =
  counter #duplicate_entry_create
    .& lbl @"model" @Text
    .& build

query_drain_latency :: PromRep 'Histogram "query_drain_latency" '[ '("action", Text)]
query_drain_latency =
  histogram #query_drain_latency
    .& lbl @"action" @Text
    .& build

rider_process_latency :: PromRep 'Histogram "rider_process_latency" '[ '("process_name", Text)]
rider_process_latency =
  histogram #rider_process_latency
    .& lbl @"process_name" @Text
    .& build

rider_kv_config_decode_failure :: PromRep 'Counter "rider_kv_config_decode_failure" '[]
rider_kv_config_decode_failure =
  counter #rider_kv_config_decode_failure
    .& build

drainer_query_executes :: PromRep 'Counter "drainer_query_executes" '[ '("action", Text)]
drainer_query_executes =
  counter #drainer_query_executes
    .& lbl @"action" @Text
    .& build

drainer_stop_status :: PromRep 'Gauge "drainer_stop_status" '[]
drainer_stop_status =
  gauge #drainer_stop_status
    .& build

rider_kafka_push_failure :: PromRep 'Counter "rider_kafka_push_failure" '[ '("action", Text), '("model", Text)]
rider_kafka_push_failure =
  counter #rider_kafka_push_failure
    .& lbl @"action" @Text
    .& lbl @"model" @Text
    .& build

rider_kafka_update_missing :: PromRep 'Counter "rider_kafka_update_missing" '[]
rider_kafka_update_missing =
  counter #rider_kafka_update_missing
    .& build

batch_fallback_used :: PromRep 'Counter "batch_fallback_used" '[]
batch_fallback_used =
  counter #batch_fallback_used
    .& build

batch_execution_time :: PromRep 'Histogram "batch_execution_time" '[ '("model", Text)]
batch_execution_time =
  histogram #batch_execution_time
    .& lbl @"model" @Text
    .& build

batch_entries_processed :: PromRep 'Counter "batch_entries_processed" '[ '("model", Text)]
batch_entries_processed =
  counter #batch_entries_processed
    .& lbl @"model" @Text
    .& build

schema_variation_alert :: PromRep 'Counter "schema_variation_alert" '[ '("model", Text)]
schema_variation_alert =
  counter #schema_variation_alert
    .& lbl @"model" @Text
    .& build
