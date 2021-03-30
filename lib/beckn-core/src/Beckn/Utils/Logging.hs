module Beckn.Utils.Logging
  ( Log (..),
    HasLogContext (..),
    LogLevel (..),
    LoggerConfig (..),
    getEulerLoggerRuntime,
    logDebug,
    logInfo,
    logWarning,
    logError,
    addLogTagToEnv,
  )
where

import Beckn.Types.Logging
import qualified Data.Aeson as A
import qualified Data.Text as Text
import qualified Data.Time as Time
import EulerHS.Prelude
import EulerHS.Runtime
import qualified EulerHS.Types as T

data LogEntry = LogEntry
  { _timestamp :: Text,
    _level :: Text,
    _tag :: Text,
    _message_number :: Text,
    _message :: Text
  }
  deriving (Generic)

instance ToJSON LogEntry where
  toJSON = genericToJSON stripAllLensPrefixOptions

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
getEulerLoggerRuntime = createLoggerRuntime logFlowFormatter . getEulerLoggerConfig

logFlowFormatter :: T.FlowFormatter
logFlowFormatter _ = do
  currTime <- Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%3Q" <$> Time.getCurrentTime
  pure $! logFormatterText (Text.pack currTime)

logFormatterText ::
  Text -> -- timestamp
  T.MessageFormatter
logFormatterText
  timestamp
  (T.PendingMsg _mbFlowGuid lvl tag msg msgNum _) = res
    where
      eulerMsg :: Text
      eulerMsg = "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""
      logEntry =
        LogEntry
          { _timestamp = timestamp,
            _level = show lvl,
            _tag = tag,
            _message_number = show msgNum,
            _message = eulerMsg
          }
      res = T.SimpleLBS $ A.encode logEntry
