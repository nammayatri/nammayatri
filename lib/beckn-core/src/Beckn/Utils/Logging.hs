module Beckn.Utils.Logging
  ( LogLevel (..),
    LoggerConfig (..),
    getEulerLoggerConfig,
  )
where

import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import qualified EulerHS.Types as T

data LogLevel = DEBUG | INFO | WARNING | ERROR
  deriving (Generic, FromDhall)

data LoggerConfig = LoggerConfig
  { isAsync :: Bool,
    level :: LogLevel,
    logToFile :: Bool,
    logFilePath :: FilePath,
    logToConsole :: Bool,
    logRawSql :: Bool
  }
  deriving (Generic, FromDhall)

toEulerLoggerConfig :: LoggerConfig -> T.LoggerConfig
toEulerLoggerConfig loggerConfig =
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

defaultEulerLoggerConfig :: FilePath -> T.LoggerConfig
defaultEulerLoggerConfig filePath =
  T.defaultLoggerConfig
    { T._isAsync = True,
      T._logToFile = True,
      T._logFilePath = filePath,
      T._logToConsole = True,
      T._logRawSql = True
    }

getEulerLoggerConfig :: FilePath -> Maybe LoggerConfig -> T.LoggerConfig
getEulerLoggerConfig filePath =
  maybe (defaultEulerLoggerConfig filePath) toEulerLoggerConfig
