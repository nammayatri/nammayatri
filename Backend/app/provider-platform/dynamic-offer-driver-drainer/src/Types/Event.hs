module Types.Event where

import EulerHS.Prelude

newtype DBSyncCounterHandler = DBSyncCounterHandler
  { pubDBSyncMetric :: DBSyncMetric -> IO ()
  }

type Action = Text

type ModelName = Text

type Latency = Double

type Stream = Text

type StreamLength = Double

type ProcessTime = Double

type BatchSize = Double

type ExecuteCount = Word

type Status = Double

data DBSyncMetric
  = PeekDBCommandError
  | DropDBCommandError
  | ParseDBCommandError Action ModelName
  | QueryExecutionFailure Action ModelName
  | DuplicateEntryCreate ModelName
  | QueryDrainLatency Action Latency
  | DrainerQueryExecutes Action Word
  | DrainerStopStatus Status
  | KafkaUpdateMissing
  | KafkaPushFailure
