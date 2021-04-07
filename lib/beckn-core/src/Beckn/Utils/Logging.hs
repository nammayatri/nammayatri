module Beckn.Utils.Logging
  ( Log (..),
    LogLevel (..),
    LoggerConfig (..),
    getEulerLoggerRuntime,
    logTagDebug,
    logTagInfo,
    logTagWarning,
    logTagError,
    logDebug,
    logInfo,
    logWarning,
    logError,
  )
where

import Beckn.Types.Logging
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as Txt
import qualified Data.Time as Time
import EulerHS.Prelude
import EulerHS.Runtime
import qualified EulerHS.Types as T
import System.Environment

logTagDebug :: Log m => Text -> Text -> m ()
logTagDebug tag = withLogContext tag . logOutput DEBUG

logTagInfo :: Log m => Text -> Text -> m ()
logTagInfo tag = withLogContext tag . logOutput INFO

logTagWarning :: Log m => Text -> Text -> m ()
logTagWarning tag = withLogContext tag . logOutput WARNING

logTagError :: Log m => Text -> Text -> m ()
logTagError tag = withLogContext tag . logOutput ERROR

logDebug :: Log m => Text -> m ()
logDebug = logOutput DEBUG

logInfo :: Log m => Text -> m ()
logInfo = logOutput INFO

logWarning :: Log m => Text -> m ()
logWarning = logOutput WARNING

logError :: Log m => Text -> m ()
logError = logOutput ERROR

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
  hostname <- (Txt.pack <$>) <$> lookupEnv "POD_NAME"
  pure $! logFormatterText currTime hostname

logFormatterText :: Time.UTCTime -> Maybe Text -> T.MessageFormatter
logFormatterText
  timestamp
  hostname
  (T.PendingMsg _mbFlowGuid elvl eTag msg msgNum logContHM) = res
    where
      logCont = HM.lookupDefault "" "log_context" logContHM
      tag = if null eTag || eTag == "\"\"" then "" else "[" <> eTag <> "]"
      lvl = case elvl of
        T.Debug -> DEBUG
        T.Warning -> WARNING
        T.Info -> INFO
        T.Error -> ERROR
      textToLBS = LBS.fromStrict . Txt.encodeUtf8
      res =
        T.SimpleLBS $
          "\""
            <> show timestamp
            <> "\" "
            <> show lvl
            <> " "
            <> show msgNum
            <> "> Hostname:\""
            <> show hostname
            <> "\", "
            <> " Context:\""
            <> textToLBS logCont
            <> textToLBS tag
            <> "\", Message:\""
            <> textToLBS msg
            <> "\""
