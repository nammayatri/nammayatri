module Beckn.Utils.Logging where

import Beckn.Types.Logging
import EulerHS.Prelude
import EulerHS.Runtime
import qualified EulerHS.Types as T

logDebug :: Log m => Text -> Text -> m ()
logDebug tag = logOutput DEBUG [tag]

logInfo :: Log m => Text -> Text -> m ()
logInfo tag = logOutput INFO [tag]

logWarning :: Log m => Text -> Text -> m ()
logWarning tag = logOutput WARNING [tag]

logError :: Log m => Text -> Text -> m ()
logError tag = logOutput ERROR [tag]

getEulerLoggerConfig :: LoggerConfig -> T.LoggerConfig
getEulerLoggerConfig loggerConfig =
  T.defaultLoggerConfig
    { T._isAsync = isAsync loggerConfig,
      T._logLevel = logLevel,
      T._logToFile = logToFile loggerConfig,
      T._logFilePath = logFilePath loggerConfig,
      T._logToConsole = logToConsole loggerConfig,
      T._logRawSql = logSql
    }
  where
    logLevel = case level loggerConfig of
      DEBUG -> T.Debug
      INFO -> T.Info
      WARNING -> T.Warning
      ERROR -> T.Error
    logSql =
      if logRawSql loggerConfig
        then T.UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION
        else T.SafelyOmitSqlLogs

getEulerLoggerRuntime :: LoggerConfig -> IO LoggerRuntime
getEulerLoggerRuntime = createLoggerRuntime T.defaultFlowFormatter . getEulerLoggerConfig
