module Beckn.Utils.IOLogging
  ( LoggerConfig (..),
    logOutputImplementation,
    withLogTagImplementation,
    logOutputIO,
    appendLogTag,
  )
where

import Beckn.Types.Logging
import Beckn.Types.Time
import qualified Data.Text as T
import qualified Data.Time as Time
import EulerHS.Prelude
import System.Log.FastLogger

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
