module Beckn.Utils.Logging where

import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import qualified EulerHS.Types as T

data LogLevel = DEBUG | INFO | WARNING | ERROR
  deriving (Generic, FromDhall)

class HasLogContext env where
  getLogContext :: env -> [Text]
  setLogContext :: [Text] -> env -> env

addLogTagToEnv :: HasLogContext env => Text -> env -> env
addLogTagToEnv tag = getLogContext >>= setLogContext . (++ [tag])

class Log m where
  logOutput :: LogLevel -> [Text] -> Text -> m ()

  logDebug :: Log m => Text -> Text -> m ()
  logDebug tag = logOutput DEBUG [tag]

  logInfo :: Log m => Text -> Text -> m ()
  logInfo tag = logOutput INFO [tag]

  logWarning :: Log m => Text -> Text -> m ()
  logWarning tag = logOutput WARNING [tag]

  logError :: Log m => Text -> Text -> m ()
  logError tag = logOutput ERROR [tag]

data LoggerConfig = LoggerConfig
  { isAsync :: Bool,
    level :: LogLevel,
    logToFile :: Bool,
    logFilePath :: FilePath,
    logToConsole :: Bool,
    logRawSql :: Bool
  }
  deriving (Generic, FromDhall)

getEulerLoggerConfig :: LoggerConfig -> T.LoggerConfig
getEulerLoggerConfig loggerConfig =
  T.defaultLoggerConfig
    { T._isAsync = isAsync loggerConfig,
      T._level = logLevel,
      T._logToFile = logToFile loggerConfig,
      T._logFilePath = logFilePath loggerConfig,
      T._logToConsole = logToConsole loggerConfig,
      T._logRawSql = logRawSql loggerConfig
    }
  where
    logLevel = case level loggerConfig of
      DEBUG -> T.Debug
      INFO -> T.Info
      WARNING -> T.Warning
      ERROR -> T.Error
