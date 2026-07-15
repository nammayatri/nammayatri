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
-- then a @metaTrackerPollMs@ delay. Enable/disable via @metaTrackerEnabled@ in
-- App.hs. Per-pod: for fleet-wide single execution, wrap the tick in a Redis lock.
module WhatsappBot.Adapter.Tracker (startWhatsAppTracker) where

import Environment
import Kernel.Prelude
import Kernel.Utils.Common (logError)
import Kernel.Utils.Service (startService)
import WhatsappBot.Adapter.Env (buildTrackerDeps)
import WhatsappBot.Tracker (trackerTick)

startWhatsAppTracker :: Flow ()
startWhatsAppTracker = do
  pollMs <- asks (.metaTrackerPollMs)
  startService "whatsapp ride tracker" $ do
    merchants <- asks (.metaWebhookMerchants)
    forM_ merchants tickMerchant
    threadDelay (pollMs * 1000)

-- | One merchant's tracker pass, isolated so a failure (Redis/DB/merchant-resolve)
-- doesn't abort the other merchants this tick.
tickMerchant :: MetaWebhookMerchant -> Flow ()
tickMerchant wm =
  (buildTrackerDeps wm >>= trackerTick)
    `catch` \(e :: SomeException) ->
      logError $ "whatsapp tracker: merchant " <> wm.merchantShortId <> " tick failed: " <> show e
