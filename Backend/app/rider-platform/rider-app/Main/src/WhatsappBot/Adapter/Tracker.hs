{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Background ride-tracker loop (@tracking/ride-tracker.ts@ + the App.hs timer),
-- co-hosted in rider-app-exe. 'startService' supplies the shutdown-aware loop with
-- a per-tick catch-log-continue; one tick = one pass over every configured merchant
-- (each isolated in its own catch so one merchant's failure never skips the rest),
-- then a delay derived fresh each tick (see 'nextPollMs'). Enable/disable via
-- @metaTrackerEnabled@ in App.hs. Per-pod: for fleet-wide single execution,
-- wrap the tick in a Redis lock.
module WhatsappBot.Adapter.Tracker (startWhatsAppTracker) where

import qualified Domain.Types.MetaWebhookConfig as DMWC
import Environment
import Kernel.Prelude
import Kernel.Utils.Common (logError)
import Kernel.Utils.Service (startService)
import qualified Storage.Queries.MetaWebhookConfig as QMWC
import WhatsappBot.Adapter.Env (buildTrackerDeps)
import WhatsappBot.Tracker (trackerTick)

-- | Fallback used ONLY when zero merchants are currently enabled — there is
-- no per-merchant trackerPollMs to take a minimum of, so the loop just
-- checks back at a fixed cadence for a merchant becoming enabled. Not a
-- business-tunable value (nobody configures "how fast should we notice a
-- newly enabled merchant"), just an idle-loop bootstrap constant.
noMerchantsPollMs :: Int
noMerchantsPollMs = 5000

startWhatsAppTracker :: Flow ()
startWhatsAppTracker =
  startService "whatsapp ride tracker" $ do
    -- Queried fresh every tick (not the CachedQueries/Redis path) rather than
    -- cached — this loop already runs on a short cadence, so a fresh read
    -- keeps newly enabled/disabled rows (and their trackerPollMs) picked up
    -- promptly instead of waiting out a cache TTL.
    --
    -- Filtered here, not by the DB: `enabled` is a boolean, so a DB-side
    -- filter can't use an index either way (see MetaWebhookConfigExtra.hs).
    allConfigs <- QMWC.findAll
    let configs = filter (.enabled) allConfigs
    forM_ configs tickMerchant
    -- Shared loop, one sleep for everyone: run at whichever enabled
    -- merchant wants the fastest updates, so nobody's configured pace is
    -- silently ignored.
    let pollMs = case map (.botConfig.trackerPollMs) configs of
          [] -> noMerchantsPollMs
          ms -> minimum ms
    threadDelay (pollMs * 1000)

-- | One merchant's tracker pass, isolated so a failure (Redis/DB/merchant-resolve)
-- doesn't abort the other merchants this tick.
tickMerchant :: DMWC.MetaWebhookConfig -> Flow ()
tickMerchant cfg =
  (buildTrackerDeps cfg >>= trackerTick)
    `catch` \(e :: SomeException) ->
      logError $ "whatsapp tracker: merchant " <> cfg.botConfig.merchantLabel <> " tick failed: " <> show e
