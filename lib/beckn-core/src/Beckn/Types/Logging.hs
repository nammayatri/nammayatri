module Beckn.Types.Logging where

import Beckn.Types.Flow
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Flow
import EulerHS.Language as L
import EulerHS.Prelude

data LogLevel = DEBUG | INFO | WARNING | ERROR
  deriving (Generic, Show, FromDhall, ToJSON)

class Log m where
  logOutput :: LogLevel -> Text -> m ()
  withLogContext :: Text -> m a -> m a

data LoggerConfig = LoggerConfig
  { isAsync :: Bool,
    level :: LogLevel,
    logToFile :: Bool,
    logFilePath :: FilePath,
    logToConsole :: Bool,
    logRawSql :: Bool
  }
  deriving (Generic, FromDhall)

instance Log (FlowR r) where
  logOutput logLevel message =
    case logLevel of
      DEBUG -> L.logDebug ("" :: Text) message
      INFO -> L.logInfo ("" :: Text) message
      WARNING -> L.logWarning ("" :: Text) message
      ERROR -> L.logError ("" :: Text) message
  withLogContext lc flowR =
    let f = runReaderT flowR
     in ReaderT $ \v -> withModifiedRuntime (addLogContext lc) $ f v
