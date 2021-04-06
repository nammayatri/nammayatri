module Beckn.Types.Logging where

import Beckn.Types.Flow
import Beckn.Utils.Dhall (FromDhall)
import qualified Data.Text as T
import EulerHS.Language as L
import EulerHS.Prelude

data LogLevel = DEBUG | INFO | WARNING | ERROR
  deriving (Generic, FromDhall)

class HasLogContext env where
  getLogContext :: env -> [Text]
  setLogContext :: [Text] -> env -> env

class Log m where
  logOutput :: LogLevel -> [Text] -> Text -> m ()
  addLogTag :: Text -> m a -> m a

data LoggerConfig = LoggerConfig
  { isAsync :: Bool,
    level :: LogLevel,
    logToFile :: Bool,
    logFilePath :: FilePath,
    logToConsole :: Bool,
    logRawSql :: Bool
  }
  deriving (Generic, FromDhall)

instance HasLogContext r => Log (FlowR r) where
  logOutput logLevel tags message =
    case logLevel of
      DEBUG -> logWithFormat L.logDebug tags message
      INFO -> logWithFormat L.logInfo tags message
      WARNING -> logWithFormat L.logWarning tags message
      ERROR -> logWithFormat L.logError tags message

  addLogTag = local . addLogTagToEnv

tagsToText :: [Text] -> Text
tagsToText = T.concat . map block
  where
    block x = "[" <> x <> "]"

logWithFormat ::
  ( MonadReader env m,
    HasLogContext env
  ) =>
  (Text -> Text -> m ()) ->
  [Text] ->
  Text ->
  m ()
logWithFormat logFunction tags msg = do
  existingTags <- asks getLogContext
  logFunction (tagsToText (existingTags ++ tags)) msg

addLogTagToEnv :: HasLogContext env => Text -> env -> env
addLogTagToEnv tag = getLogContext >>= setLogContext . (++ [tag])
