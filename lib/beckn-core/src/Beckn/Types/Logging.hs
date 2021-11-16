module Beckn.Types.Logging where

import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import GHC.Records.Extra
import System.Log.FastLogger

data LogLevel = DEBUG | INFO | WARNING | ERROR
  deriving (Generic, Show, Eq, Ord, FromDhall, ToJSON)

type HasLog r = HasField "loggerEnv" r LoggerEnv

class Log m where
  logOutput :: LogLevel -> Text -> m ()
  withLogTag :: Text -> m a -> m a

data LoggerConfig = LoggerConfig
  { level :: LogLevel,
    logToFile :: Bool,
    logFilePath :: FilePath,
    logToConsole :: Bool,
    logRawSql :: Bool
  }
  deriving (Generic, FromDhall)

data Logger = Logger
  { printLogFunc :: FastLogger,
    cleanUpFunc :: IO ()
  }

data LoggerEnv = LoggerEnv
  { level :: LogLevel,
    hostName :: Maybe Text,
    tags :: [Text],
    fileLogger :: Maybe Logger,
    consoleLogger :: Maybe Logger
  }

prepareLoggerEnv :: LoggerConfig -> Maybe Text -> IO LoggerEnv
prepareLoggerEnv loggerConfig hostName = do
  fileLogger <-
    if not loggerConfig.logToFile
      then return Nothing
      else Just <$> prepareLogger (LogFileNoRotate loggerConfig.logFilePath defaultBufSize)

  consoleLogger <-
    if not loggerConfig.logToFile
      then return Nothing
      else Just <$> prepareLogger (LogStdout defaultBufSize)

  return $
    LoggerEnv
      { level = loggerConfig.level,
        tags = [],
        ..
      }
  where
    prepareLogger logType = do
      (printLogFunc, cleanUpFunc) <- newFastLogger logType
      return $ Logger {..}

releaseLoggerEnv :: LoggerEnv -> IO ()
releaseLoggerEnv LoggerEnv {..} = do
  whenJust fileLogger $ \logger -> logger.cleanUpFunc
  whenJust consoleLogger $ \logger -> logger.cleanUpFunc