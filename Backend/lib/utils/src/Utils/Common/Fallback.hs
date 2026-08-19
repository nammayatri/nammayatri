module Utils.Common.Fallback (withFallback) where

import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
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
