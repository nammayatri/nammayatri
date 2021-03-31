module Beckn.Types.Logging where

import Beckn.Types.Flow
import Beckn.Utils.Flow
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Language as L
import EulerHS.Prelude

data LogLevel = DEBUG | INFO | WARNING | ERROR
  deriving (Generic, FromDhall)

class Log m where
  logOutput :: LogLevel -> [Text] -> Text -> m ()
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
  logOutput logLevel tag message =
    case logLevel of
      DEBUG -> L.logDebug tag message
      INFO -> L.logInfo tag message
      WARNING -> L.logWarning tag message
      ERROR -> L.logError tag message
  withLogContext lc flowR =
    let f = runReaderT flowR
    in ReaderT $ \v -> L.withModifiedRuntime (addLogContext lc) $ f v
