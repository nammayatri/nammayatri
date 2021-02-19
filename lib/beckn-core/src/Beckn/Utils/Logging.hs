module Beckn.Utils.Logging
  ( LogLevel (..),
    LoggerConfig (..),
    Log (..),
    HasLogContext (..),
    getEulerLoggerConfig,
    addLogTagToEnv,
  )
where

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
  logDebug :: Text -> Text -> m ()
  logInfo :: Text -> Text -> m ()
  logWarning :: Text -> Text -> m ()
  logError :: Text -> Text -> m ()

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
