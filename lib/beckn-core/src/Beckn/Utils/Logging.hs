module Beckn.Utils.Logging
  ( Log (..),
    LogLevel (..),
    log,
    logTagDebug,
    logTagInfo,
    logTagWarning,
    logTagError,
    logDebug,
    logInfo,
    logWarning,
    logError,
    withTransactionIdLogTag,
    withPersonIdLogTag,
    makeLogSomeException,
    logPretty,
  )
where

import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Id
import Beckn.Types.Logging
import Beckn.Utils.GenericPretty (PrettyShow, textPretty)
import EulerHS.Prelude
import GHC.Records.Extra

log :: Log m => LogLevel -> Text -> m ()
log = logOutput

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

withPersonIdLogTag :: Log m => Id b -> m a -> m a
withPersonIdLogTag personId = do
  withLogTag ("actor-" <> getId personId)

withTransactionIdLogTag :: (HasField "context" b c, HasField "transaction_id" c Text, Log m) => b -> m a -> m a
withTransactionIdLogTag req =
  withLogTag ("txnId-" <> req.context.transaction_id)

makeLogSomeException :: SomeException -> Text
makeLogSomeException someExc
  | Just (HTTPException err) <- fromException someExc = logHTTPError err
  | Just (BaseException err) <- fromException someExc =
    fromMaybe (show err) $ toMessage err
  | otherwise = show someExc
  where
    logHTTPError err =
      show (toHttpCode err)
        <> " "
        <> toErrorCode err
        <> maybe "" (": " <>) (toMessage err)

renderViaShow :: (Show a) => Text -> a -> Text
renderViaShow description val = description <> ": " <> show val

renderViaPrettyShow :: (PrettyShow a) => Text -> a -> Text
renderViaPrettyShow description val = description <> "\n" <> textPretty val

logPretty ::
  ( PrettyShow a,
    Show a,
    Log m,
    MonadReader env m,
    HasField "config" env conf,
    HasField "loggerConfig" conf LoggerConfig
  ) =>
  LogLevel ->
  Text ->
  a ->
  m ()
logPretty logLevel description val = do
  pretty <- asks (.config.loggerConfig.prettyPrinting)
  let render =
        if pretty
          then renderViaPrettyShow
          else renderViaShow
  logOutput logLevel $ render description val
