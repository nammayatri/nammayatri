module Constants where

import EulerHS.Prelude
import Types.DBSync (History)

nodeEnvironmentEnvKey :: [Char]
nodeEnvironmentEnvKey = "NODE_ENV"

--                                  |
kvRedis :: [Char]
kvRedis = "KVRedis"

--                                  |
dbsyncConfigKey :: Text
dbsyncConfigKey = "driver-dbsync-config"

--                                  |
ecRedisDBStream :: [Char]
ecRedisDBStream = "driver-db-sync-stream"

--                                  |
ecRedisDBStreamCounter :: [Char]
ecRedisDBStreamCounter = "driver-db-sync-stream-counter"

--                                  |
ecRedisErrorStream :: [Char]
ecRedisErrorStream = "driver-db-sync-error-stream"

--                                  |
ecRedisFailedStream :: [Char]
ecRedisFailedStream = "driver-db-sync-failed-stream"

--                                  |
numberOfStreamsForKV :: Integer
numberOfStreamsForKV = 128

--                                  |
emptyHistory :: History
emptyHistory = []

--                                  |
drainerStopKey :: Text
drainerStopKey = "DRIVER_DRAINER_STOP"

--                                  |
forceDrainEnabledKey :: Text
forceDrainEnabledKey = "DRIVER_FORCE_DRAIN"

--                                  |
envNamePrefix :: String
envNamePrefix = "DRAINER"

--                                  |
envNameDelim :: String
envNameDelim = "_"

--                                  |
dbSyncStreamEnvKey :: String
dbSyncStreamEnvKey = "DBSYNC_STREAM"

--                                  |
redisHostEnvKey :: String
redisHostEnvKey = "REDIS_HOST"

--                                  |
redisPortEnvKey :: String
redisPortEnvKey = "REDIS_PORT"

--                                  |
redisPasswordReqEnvKey :: String
redisPasswordReqEnvKey = "REDIS_PASSWORD_REQ"

--                                  |
redisAuthEnvKey :: String
redisAuthEnvKey = "REDIS_AUTH"

--                                  |
redisDbValueEnvKey :: String
redisDbValueEnvKey = "REDIS_DB_VAL"

--                                  |
redisSocketKeepAliveEnvKey :: String
redisSocketKeepAliveEnvKey = "REDIS_SOCKET_KEEP_ALIVE"

--                                  |
dbPoolEnvKey :: String
dbPoolEnvKey = "DB_POOL"

--                                  |
dbMaxIdleTimeoutEnvKey :: String
dbMaxIdleTimeoutEnvKey = "DB_MAX_IDLE_TIMEOUT"

--                                  |
dbMaxConnectionsEnvKey :: String
dbMaxConnectionsEnvKey = "DB_MAX_CONNECTIONS"

--                                  |
dbHostEnvKey :: String
dbHostEnvKey = envNamePrefix <> envNameDelim <> "DB_HOST"

--                                  |
dbPortEnvKey :: String
dbPortEnvKey = envNamePrefix <> envNameDelim <> "DB_PORT"

--                                  |
dbUserEnvKey :: String
dbUserEnvKey = envNamePrefix <> envNameDelim <> "DB_USER"

--                                  |
dbNameEnvKey :: String
dbNameEnvKey = envNamePrefix <> envNameDelim <> "DB_NAME"

--                                  |
dbPasswordEnvKey :: String
dbPasswordEnvKey = envNamePrefix <> envNameDelim <> "DB_PASSWORD"

--                                  |
maskingEnabledEnvKey :: String
maskingEnabledEnvKey = "MASKING_ENABLED"

--                                  |
logFileEnvKey :: String
logFileEnvKey = "LOG_FILE"

--                                  |
logToFileEnvKey :: String
logToFileEnvKey = "LOG_TO_FILE"

--                                  |
logAsyncEnvKey :: String
logAsyncEnvKey = "LOG_ASYNC"

--                                  |
logToConsoleEnvKey :: String
logToConsoleEnvKey = "LOG_TO_CONSOLE"

--                                  |
logRawSqlEnvKey :: String
logRawSqlEnvKey = "LOG_RAW_SQL"

--                                  |
logAPIEnvKey :: String
logAPIEnvKey = "LOG_API"

--                                  |
logLevelEnvKey :: String
logLevelEnvKey = "LOG_LEVEL"

--                                  |
logFormatterEnvKey :: String
logFormatterEnvKey = "CUSTOM_LOG_FORMAT"

--                                  |
gracefulShutDownPeriodSecondsEnvKey :: String
gracefulShutDownPeriodSecondsEnvKey = "GRACEFUL_STUT_DOWN_PERIOD_SECONDS"

--                                  |
streamBlockTimeEnvKey :: String
streamBlockTimeEnvKey = "STREAM_BLOCK_TIME"

--                                  |
threadPerPodCountEnvKey :: String
threadPerPodCountEnvKey = "THREAD_PER_POD_COUNT"

--                                  |
whitelistedLoggingKeys :: Text
whitelistedLoggingKeys = "WHITELISTED_LOGGING_KEYS"

--                                  |
defaultGracefulShutDownPeriodInMs :: Int
defaultGracefulShutDownPeriodInMs = 0

--                                  |
dbFailureRetryDelayEnvKey :: String
dbFailureRetryDelayEnvKey = "DB_FAILURE_RETRY_DELAY"

--                                  |
drainerFailureRetryDelayEnvKey :: String
drainerFailureRetryDelayEnvKey = "DRAINER_FAILURE_RETRY_DELAY"

--                                  |
maxDbFailureRetries :: String
maxDbFailureRetries = "MAX_DB_FAILURE_RETRIES"

--

drainerExecutionDelayEnvKey :: String
drainerExecutionDelayEnvKey = "DRAINER_EXECUTION_DELAY"

threadPerPodCount :: String
threadPerPodCount = "THREAD_PER_POD_COUNT"
