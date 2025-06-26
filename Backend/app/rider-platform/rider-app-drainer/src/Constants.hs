module Constants where

import EulerHS.Prelude
import Types.DBSync (History)

kvRedis :: [Char]
kvRedis = "KVRedis"

dbsyncConfigKey :: Text
dbsyncConfigKey = "rider-dbsync-config"

ecRedisDBStream :: [Char]
ecRedisDBStream = "rider-db-sync-stream"

ecRedisDBStreamCounter :: [Char]
ecRedisDBStreamCounter = "rider-db-sync-stream-counter"

ecRedisErrorStream :: [Char]
ecRedisErrorStream = "rider-db-sync-error-stream"

ecRedisFailedStream :: [Char]
ecRedisFailedStream = "rider-db-sync-failed-stream"

numberOfStreamsForKV :: Integer
numberOfStreamsForKV = 128

emptyHistory :: History
emptyHistory = []

drainerStopKey :: Text
drainerStopKey = "RIDER_DRAINER_STOP"

forceDrainEnabledKey :: Text
forceDrainEnabledKey = "FORCE_DRAIN"

dbSyncStreamEnvKey :: String
dbSyncStreamEnvKey = "DBSYNC_STREAM"

gracefulShutDownPeriodSecondsEnvKey :: String
gracefulShutDownPeriodSecondsEnvKey = "GRACEFUL_STUT_DOWN_PERIOD_SECONDS"

streamBlockTimeEnvKey :: String
streamBlockTimeEnvKey = "STREAM_BLOCK_TIME"

defaultGracefulShutDownPeriodInMs :: Int
defaultGracefulShutDownPeriodInMs = 0

dbFailureRetryDelayEnvKey :: String
dbFailureRetryDelayEnvKey = "DB_FAILURE_RETRY_DELAY"

drainerFailureRetryDelayEnvKey :: String
drainerFailureRetryDelayEnvKey = "DRAINER_FAILURE_RETRY_DELAY"

maxDbFailureRetries :: String
maxDbFailureRetries = "MAX_DB_FAILURE_RETRIES"

drainerExecutionDelayEnvKey :: String
drainerExecutionDelayEnvKey = "DRAINER_EXECUTION_DELAY"

threadPerPodCount :: String
threadPerPodCount = "THREAD_PER_POD_COUNT"

pushToKafkaEnvKey :: String
pushToKafkaEnvKey = "PUSH_TO_KAFKA"

kafkaUpdateFailedStream :: String
kafkaUpdateFailedStream = "rider-kafka-update-failed-stream"

-- DB Connection Recovery Constants
dbConnectionRetryMaxAttempts :: String
dbConnectionRetryMaxAttempts = "DB_CONNECTION_RETRY_MAX_ATTEMPTS"

dbConnectionRetryDelayEnvKey :: String
dbConnectionRetryDelayEnvKey = "DB_CONNECTION_RETRY_DELAY"

-- Default values for DB connection recovery
defaultDbConnectionRetryMaxAttempts :: Int
defaultDbConnectionRetryMaxAttempts = 5

defaultDbConnectionRetryDelay :: Int
defaultDbConnectionRetryDelay = 2000000 -- 2 seconds in microseconds
