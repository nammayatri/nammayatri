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

-- euler_db_sync_stream_length :: PromRep 'Gauge "euler_db_sync_stream_length" '[ '("stream", Text)]
-- euler_db_sync_stream_length =
--   gauge #euler_db_sync_stream_length
--     .& lbl @"stream" @Text
--     .& build

-- euler_query_batch_process_time :: PromRep 'Histogram "euler_query_batch_process_time" '[]
-- euler_query_batch_process_time =
--   histogram #euler_query_batch_process_time
--     .& build

-- euler_query_batch_size :: PromRep 'Gauge "euler_query_batch_size" '[]
-- euler_query_batch_size =
--   gauge #euler_query_batch_size
--     .& build

drainer_query_executes :: PromRep 'Counter "drainer_query_executes" '[ '("action", Text)]
drainer_query_executes =
  counter #drainer_query_executes
    .& lbl @"action" @Text
    .& build

drainer_stop_status :: PromRep 'Gauge "drainer_stop_status" '[]
drainer_stop_status =
  gauge #drainer_stop_status
    .& build

rider_kafka_push_failure :: PromRep 'Counter "rider_kafka_push_failure" '[]
rider_kafka_push_failure =
  counter #rider_kafka_push_failure
    .& build
