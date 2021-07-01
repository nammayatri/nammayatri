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
    appendLogContext,
    withTransactionIdLogTag,
    logOutputImplementation,
    withLogTagImplementation,
    withTransactionIdLogTagMig,
    makeLogSomeException,
  )
where

import Beckn.Types.Core.Context (Context (transaction_id))
import qualified Beckn.Types.Core.Migration.Context as M.Context
import Beckn.Types.Error.BaseError.HTTPError.APIError
import Beckn.Types.Error.BaseError.HTTPError.BecknAPIError
import Beckn.Types.Logging
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as Txt
import qualified Data.Time as Time
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Runtime
import EulerHS.Types (LogContext)
import qualified EulerHS.Types as T
import GHC.Records.Extra
import qualified Prelude as P

logOutputImplementation :: L.MonadFlow m => LogLevel -> T.Message -> m ()
logOutputImplementation logLevel message =
  case logLevel of
    DEBUG -> L.logDebug EmtpyTag message
    INFO -> L.logInfo EmtpyTag message
    WARNING -> L.logWarning EmtpyTag message
    ERROR -> L.logError EmtpyTag message

withLogTagImplementation ::
  L.MonadFlow m =>
  Text ->
  ReaderT r L.Flow a ->
  ReaderT r m a
withLogTagImplementation lc flowR =
  ReaderT $
    L.withLoggerContext (appendLogContext lc)
      . runReaderT flowR

data EmtpyTag = EmtpyTag

instance P.Show EmtpyTag where
  show _ = ""

logTagDebug :: Log m => Text -> Text -> m ()
logTagDebug tag = withLogTag tag . logOutput DEBUG

logTagInfo :: Log m => Text -> Text -> m ()
logTagInfo tag = withLogTag tag . logOutput INFO

logTagWarning :: Log m => Text -> Text -> m ()
logTagWarning tag = withLogTag tag . logOutput WARNING

logTagError :: Log m => Text -> Text -> m ()
logTagError tag = withLogTag tag . logOutput ERROR

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

getEulerLoggerRuntime :: Maybe Text -> LoggerConfig -> IO LoggerRuntime
getEulerLoggerRuntime hostname = createLoggerRuntime (logFlowFormatter hostname) . getEulerLoggerConfig

logFlowFormatter :: Maybe Text -> T.FlowFormatter
logFlowFormatter hostname _ = do
  currTime <- Time.getCurrentTime
  pure $ logFormatterText currTime hostname

logFormatterText :: Time.UTCTime -> Maybe Text -> T.MessageFormatter
logFormatterText
  timestamp
  hostname
  (T.PendingMsg _mbFlowGuid elvl eTag msg msgNum logContHM) = res
    where
      logCont = HM.lookupDefault "" logContextKey logContHM
      tag = if null eTag || eTag == "\"\"" then "" else formatTag eTag
      lvl = case elvl of
        T.Debug -> DEBUG
        T.Warning -> WARNING
        T.Info -> INFO
        T.Error -> ERROR
      textToLBS = LBS.fromStrict . Txt.encodeUtf8
      res =
        T.SimpleLBS $
          show timestamp
            <> " "
            <> show lvl
            <> " "
            <> show msgNum
            <> "> @"
            <> textToLBS (fromMaybe "null" hostname)
            <> " "
            <> textToLBS logCont
            <> textToLBS tag
            <> " |> "
            <> textToLBS msg

formatTag :: Text -> Text
formatTag tag = "[" <> tag <> "]"

appendLogContext :: Text -> LogContext -> LogContext
appendLogContext val lc =
  let oldLCText = fromMaybe "" $ HM.lookup logContextKey lc
   in HM.insert logContextKey (oldLCText <> formatTag val) lc

logContextKey :: Text
logContextKey = "log_context"

withTransactionIdLogTag :: (HasField "context" b Context, Log m) => b -> m a -> m a
withTransactionIdLogTag req = do
  let context = req.context
      transaction_id_ = transaction_id context
  withLogTag ("txnId-" <> transaction_id_)

withTransactionIdLogTagMig :: (HasField "context" b M.Context.Context, Log m) => b -> m a -> m a
withTransactionIdLogTagMig req = do
  let context = req.context
      transaction_id_ = M.Context.transaction_id context
  withLogTag ("txnId-" <> transaction_id_)

makeLogSomeException :: SomeException -> Text
makeLogSomeException someExc
  | Just (APIException err) <- fromException someExc = logHTTPError err
  | Just (BecknAPIException err) <- fromException someExc = logHTTPError err
  | Just (BaseException err) <- fromException someExc = show err
  | otherwise = show someExc
  where
    logHTTPError err =
      show (toHttpCode err)
        <> " "
        <> toErrorCode err
        <> maybe "" (": " <>) (toMessage err)
