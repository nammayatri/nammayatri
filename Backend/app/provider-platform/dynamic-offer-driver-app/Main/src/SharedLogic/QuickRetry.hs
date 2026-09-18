{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.QuickRetry (withQuickRetry) where

import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics (..))
import Kernel.Utils.Common

-- | Retry tuned for intra-VPC service calls (LTS, the BAP internal API). The kernel's
-- withShortRetry backoff (4s/8s/16s sleeps) is sized for Beckn WAN hops; measured on the
-- one-shot assignment callback, those sleeps dominated the latency tail while the
-- connection-level failures they guard against recovered on the very next attempt — so
-- retry after 0.5s / 1s / 2s instead. Same failure classes as the kernel wrapper
-- (connection errors and 503 only, via catchConnectionErrors) and the same
-- url_call_retries/retry_failures metrics, so existing dashboards keep working.
-- Trade-off vs withShortRetry: the recoverable-outage window shrinks from ~28s to ~3.5s;
-- use only for internal targets where waiting half a minute inline is worse than failing
-- into the caller's error path.
withQuickRetry :: (MonadCatch m, MonadIO m, Log m, CoreMetrics m) => m a -> m a
withQuickRetry action = go 1
  where
    delaysMs :: [Int]
    delaysMs = [500, 1000, 2000]
    go attempt =
      catchConnectionErrors action $ \err ->
        case drop (attempt - 1) delaysMs of
          (delayMs : _) -> do
            logWarning $ "Quick-retry attempt " <> show attempt <> " failed with a connection-class error, retrying in " <> show delayMs <> "ms"
            addUrlCallRetries err.baseUrl attempt
            threadDelay (delayMs * 1000)
            go (attempt + 1)
          [] -> do
            addUrlCallRetryFailures err.baseUrl
            throwM err
