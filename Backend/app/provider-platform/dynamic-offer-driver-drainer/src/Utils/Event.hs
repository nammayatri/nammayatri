{-# LANGUAGE OverloadedLabels #-}

module Utils.Event where

import Data.Text
import Euler.Events.MetricApi.MetricApi

euler_peek_db_command_error :: PromRep 'Counter "euler_peek_db_command_error" '[]
euler_peek_db_command_error =
  counter #euler_peek_db_command_error
    .& build

euler_drop_db_command_error :: PromRep 'Counter "euler_drop_db_command_error" '[]
euler_drop_db_command_error =
  counter #euler_drop_db_command_error
    .& build

euler_parse_db_command_error :: PromRep 'Counter "euler_parse_db_command_error" '[ '("action", Text), '("model_object", Text)]
euler_parse_db_command_error =
  counter #euler_parse_db_command_error
    .& lbl @"action" @Text
    .& lbl @"model_object" @Text
    .& build

euler_query_execution_failure_error :: PromRep 'Counter "euler_query_execution_failure_error" '[ '("action", Text), '("model", Text)]
euler_query_execution_failure_error =
  counter #euler_query_execution_failure_error
    .& lbl @"action" @Text
    .& lbl @"model" @Text
    .& build

euler_duplicate_entry_create :: PromRep 'Counter "euler_duplicate_entry_create" '[ '("model", Text)]
euler_duplicate_entry_create =
  counter #euler_duplicate_entry_create
    .& lbl @"model" @Text
    .& build

euler_query_drain_latency :: PromRep 'Histogram "euler_query_drain_latency" '[ '("action", Text)]
euler_query_drain_latency =
  histogram #euler_query_drain_latency
    .& lbl @"action" @Text
    .& build

euler_db_sync_stream_length :: PromRep 'Gauge "euler_db_sync_stream_length" '[ '("stream", Text)]
euler_db_sync_stream_length =
  gauge #euler_db_sync_stream_length
    .& lbl @"stream" @Text
    .& build

euler_query_batch_process_time :: PromRep 'Histogram "euler_query_batch_process_time" '[]
euler_query_batch_process_time =
  histogram #euler_query_batch_process_time
    .& build

euler_query_batch_size :: PromRep 'Gauge "euler_query_batch_size" '[]
euler_query_batch_size =
  gauge #euler_query_batch_size
    .& build

euler_drainer_query_executes :: PromRep 'Counter "euler_drainer_query_executes" '[ '("action", Text)]
euler_drainer_query_executes =
  counter #euler_drainer_query_executes
    .& lbl @"action" @Text
    .& build

euler_drainer_stop_status :: PromRep 'Gauge "euler_drainer_stop_status" '[]
euler_drainer_stop_status =
  gauge #euler_drainer_stop_status
    .& build
