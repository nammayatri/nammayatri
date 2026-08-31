module Utils.Common.Fallback (withFallback, withTimeoutOrRethrow) where

import qualified EulerHS.Language as L
import EulerHS.Types (AwaitingError (..), Microseconds (..))
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Forkable (Forkable, awaitableFork)
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.Logging (Log, logWarning)

withFallback ::
  (MonadCatch m, Log m, Metrics.CoreMetrics m) =>
  Text ->
  m a ->
  m a ->
  m a
withFallback tag primary fallback =
  try @_ @SomeException primary >>= \case
    Right result -> pure result
    Left err -> do
      logWarning (tag <> ": failed (" <> show err <> "), using fallback")
      Metrics.incrementGenericMetrics (tag <> "_error_fallback")
      fallback

-- | Bounds `action` to `timeoutInSec`. On success or on `action` throwing
-- normally, behaves exactly like running `action` directly — the original
-- exception (e.g. a typed domain error like PersonNotFound) is re-thrown
-- unchanged, since `try` runs *inside* the fork and its result, not the raw
-- action, is what's awaited; only euler-hs's own AwaitingTimeout/ForkedFlowError
-- ever collapse to a fresh InternalError. Use only around read-only/no-side-effect
-- actions: euler-hs has no cancellation primitive, so on timeout the forked
-- action keeps running in the background and its result is discarded — safe
-- for a read, not for a write (the write can still land after the caller has
-- already been told it failed).
withTimeoutOrRethrow ::
  (L.MonadFlow m, Forkable m, Log m, MonadThrow m) =>
  Text ->
  Int ->
  m a ->
  m a
withTimeoutOrRethrow tag timeoutInSec action = do
  awaitable <- awaitableFork tag (try @_ @SomeException action)
  L.await (Just (Microseconds (fromIntegral timeoutInSec * 1000000))) awaitable >>= \case
    Right (Right result) -> pure result
    Right (Left exc) -> throwM exc
    Left AwaitingTimeout -> do
      logWarning (tag <> ": timed out after " <> show timeoutInSec <> "s, DB likely unavailable")
      throwError (InternalError (tag <> ": timed out after " <> show timeoutInSec <> "s"))
    Left (ForkedFlowError e) -> do
      logWarning (tag <> ": forked action failed (" <> e <> ")")
      throwError (InternalError (tag <> ": " <> e))
