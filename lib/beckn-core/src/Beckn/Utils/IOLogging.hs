module Beckn.Utils.IOLogging
  ( LoggerConfig (..),
    Logger,
    LoggerEnv (..),
    HasLog,
    prepareLoggerEnv,
    releaseLoggerEnv,
    logOutputImplementation,
    withLogTagImplementation,
    logOutputIO,
    appendLogTag,
    withLoggerEnv,
  )
where

import Beckn.Prelude
import Beckn.Types.Logging
import Beckn.Types.Time
import qualified Control.Monad.Catch as C
import qualified Data.Text as T
import qualified Data.Time as Time
import System.Log.FastLogger

type HasLog r = HasField "loggerEnv" r LoggerEnv

data Logger = Logger
  { printLogFunc :: FastLogger,
    cleanUpFunc :: IO ()
  }

data LoggerEnv = LoggerEnv
  { level :: LogLevel,
    hostName :: Maybe Text,
    tags :: [Text],
    fileLogger :: Maybe Logger,
    consoleLogger :: Maybe Logger,
    logRawSql :: Bool
  }

withLoggerEnv :: LoggerConfig -> Maybe Text -> (LoggerEnv -> IO a) -> IO a
withLoggerEnv loggerConfig hostName = C.bracket (prepareLoggerEnv loggerConfig hostName) releaseLoggerEnv

prepareLoggerEnv :: LoggerConfig -> Maybe Text -> IO LoggerEnv
prepareLoggerEnv loggerConfig hostName = do
  fileLogger <-
    if loggerConfig.logToFile
      then Just <$> prepareLogger (LogFileNoRotate loggerConfig.logFilePath defaultBufSize)
      else return Nothing

  consoleLogger <-
    if loggerConfig.logToConsole
      then Just <$> prepareLogger (LogStdout defaultBufSize)
      else return Nothing

  return $
    LoggerEnv
      { level = loggerConfig.level,
        logRawSql = loggerConfig.logRawSql,
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

logOutputImplementation :: (HasLog r, MonadReader r m, MonadIO m, MonadTime m) => LogLevel -> Text -> m ()
logOutputImplementation logLevel message = do
  logEnv <- asks (.loggerEnv)
  logOutputIO logEnv logLevel message

logOutputIO :: (MonadIO m, MonadTime m) => LoggerEnv -> LogLevel -> Text -> m ()
logOutputIO logEnv logLevel message = do
  when (logLevel >= logEnv.level) $ do
    now <- getCurrentTime
    let formattedMessage = logFormatterText now logEnv.hostName logLevel logEnv.tags message
    whenJust logEnv.fileLogger $ \logger ->
      liftIO . logger.printLogFunc $ toLogStr formattedMessage
    whenJust logEnv.consoleLogger $ \logger ->
      liftIO . logger.printLogFunc $ toLogStr formattedMessage

withLogTagImplementation ::
  (HasLog r, MonadReader r m) =>
  Text ->
  m a ->
  m a
withLogTagImplementation tag = local modifyEnv
  where
    modifyEnv env = do
      let logEnv = env.loggerEnv
          updLogEnv = appendLogTag tag logEnv
      env{loggerEnv = updLogEnv}

appendLogTag :: Text -> LoggerEnv -> LoggerEnv
appendLogTag tag logEnv = do
  logEnv{tags = tag : logEnv.tags}

formatTags :: [Text] -> Text
formatTags tag = "[" <> T.intercalate ", " (reverse tag) <> "]"

logFormatterText :: Time.UTCTime -> Maybe Text -> LogLevel -> [Text] -> Text -> Text
logFormatterText timestamp hostname lvl tags msg = res
  where
    tag = if null tags then "" else formatTags tags
    res =
      show timestamp
        <> " "
        <> show lvl
        <> "> "
        <> maybe "" ("@" <>) hostname
        <> " "
        <> tag
        <> " |> "
        <> msg
        <> "\n"
