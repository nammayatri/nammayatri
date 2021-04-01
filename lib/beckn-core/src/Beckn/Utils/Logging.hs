module Beckn.Utils.Logging
  ( Log (..),
    LogLevel (..),
    LoggerConfig (..),
    getEulerLoggerRuntime,
    logDebug,
    logInfo,
    logWarning,
    logError,
  )
where

import Beckn.Types.Logging

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Time as Time
import EulerHS.Prelude
import EulerHS.Runtime
import qualified EulerHS.Types as T

data LogEntry = LogEntry
  { timestamp :: Time.UTCTime,
    level :: LogLevel,
    logContext :: Text,
    tag :: Text,
    messageNumber :: Int,
    message :: Text
  }
  deriving (Generic, ToJSON)

logDebug :: Log m => Text -> Text -> m ()
logDebug tag = logOutput DEBUG [tag]

logInfo :: Log m => Text -> Text -> m ()
logInfo tag = logOutput INFO [tag]

logWarning :: Log m => Text -> Text -> m ()
logWarning tag = logOutput WARNING [tag]

logError :: Log m => Text -> Text -> m ()
logError tag = logOutput ERROR [tag]

getEulerLoggerConfig :: LoggerConfig -> T.LoggerConfig
getEulerLoggerConfig LoggerConfig {..} =
  T.defaultLoggerConfig
    { T._isAsync = isAsync,
      T._logLevel = logLevel,
      T._logToFile = logToFile,
      T._logFilePath = logFilePath,
      T._logToConsole = logToConsole,
      T._logRawSql = logSql
    }
  where
    logLevel = case level of
      DEBUG -> T.Debug
      INFO -> T.Info
      WARNING -> T.Warning
      ERROR -> T.Error
    logSql =
      if logRawSql
        then T.UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION
        else T.SafelyOmitSqlLogs

getEulerLoggerRuntime :: LoggerConfig -> IO LoggerRuntime
getEulerLoggerRuntime = createLoggerRuntime logFlowFormatter . getEulerLoggerConfig

logFlowFormatter :: T.FlowFormatter
logFlowFormatter _ = do
  currTime <- Time.getCurrentTime
  pure $! logFormatterText currTime

logFormatterText ::
  Time.UTCTime -> -- timestamp
  T.MessageFormatter
logFormatterText
  timestamp
  (T.PendingMsg _mbFlowGuid elvl tag msg msgNum logContHM) = res
    where
      logCont = HM.lookupDefault "" "log_context" logContHM
      logEntry =
        LogEntry
          { timestamp = timestamp,
            level = lvl,
            logContext = logCont,
            tag = tag,
            messageNumber = msgNum,
            message = msg
          }
      res = T.SimpleLBS $ A.encode logEntry
      lvl = case elvl of
        T.Debug -> DEBUG
        T.Warning -> WARNING
        T.Info -> INFO
        T.Error -> ERROR