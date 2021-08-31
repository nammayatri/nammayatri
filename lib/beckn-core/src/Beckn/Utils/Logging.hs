module Beckn.Utils.Logging
  ( Log (..),
    LogLevel (..),
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
    withTransactionIdLogTagMig,
    makeLogSomeException,
  )
where

import Beckn.Types.Core.Context (Context (transaction_id))
import qualified Beckn.Types.Core.Migration.Context as M.Context
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Id
import Beckn.Types.Logging
import EulerHS.Prelude
import GHC.Records.Extra

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
