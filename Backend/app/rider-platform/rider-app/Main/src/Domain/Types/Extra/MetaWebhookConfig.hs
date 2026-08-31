{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Per-phone-number WhatsApp-bot tuning config, persisted as the @bot_config@
-- jsonb column of @meta_config@. Field-for-field copy of the old
-- Dhall-sourced @Environment.MetaBotCfg@, with an Aeson codec instead of
-- @FromDhall@ (this JSON only ever round-trips through our own DB column,
-- never the Meta wire format, so no key remapping is needed here).
module Domain.Types.Extra.MetaWebhookConfig where

import Data.Aeson
import Kernel.Prelude

data MetaBotCfg = MetaBotCfg
  { merchantLabel :: Text,
    -- Ordered list of ride-type ids this merchant offers via WhatsApp, e.g.
    -- ["flexi", "regular"] — order is PRIORITY order, not just membership.
    -- Replaces the old rideMode :: Text ("flexi"|"regular"|"both"), which
    -- could only express membership, never priority, and had no room for a
    -- 3rd ride type. Unrecognized ids are dropped (see
    -- WhatsappBot.Adapter.Env.mkMerchantCtx) rather than failing the whole
    -- config, so a not-yet-built future type named here doesn't break the bot.
    rideTypesOrder :: [Text],
    -- Per-merchant override for how many ride-type buttons show directly
    -- before falling back to "More" (see WhatsappBot.Flow.Booking.splitRideTypes).
    -- Nothing -> today's default nudge-toward-priority layout, unchanged.
    -- Just n -> if all offered types fit within n (clamped to [1,2], WhatsApp's
    -- real button ceiling), show them all directly with no "More" at all;
    -- otherwise show n directly and hide the rest.
    maxDirectButtons :: Maybe Int,
    flexiBaseFare :: Maybe Double,
    flexiPerKm :: Maybe Double,
    flexiServiceArea :: Maybe Text,
    flexiServiceRadiusKm :: Maybe Double,
    flexiRentalDistanceM :: Int,
    flexiRentalDurationS :: Int,
    flexiIntroVideoUrl :: Maybe Text,
    flexiSupportPhone :: Maybe Text,
    nyTrackingUrl :: Text,
    -- Poll-loop tuning, per merchant instead of code-hardcoded (review ask:
    -- Adapter/Env.hs's mkBotConfig used to hardcode these). Required, not
    -- Maybe: meta_config has no production rows yet (table was never
    -- deployed — see the 1558-meta-config.sql migration), so there is no
    -- existing-row backward-compat concern that would otherwise call for a
    -- Maybe-with-fallback here.
    flexiQuotePollAttempts :: Int,
    flexiQuotePollIntervalMs :: Int,
    regularEstimatePollAttempts :: Int,
    regularEstimatePollIntervalMs :: Int,
    driverPollAttempts :: Int,
    driverPollIntervalMs :: Int,
    driverPollNotifyEvery :: Int,
    -- Per-merchant instead of code-hardcoded (review ask, same as the poll
    -- constants above). sessionTtlSec: WhatsappBot.Adapter.SessionStore's
    -- refresh-on-access session TTL. trackerMaxAgeSec: WhatsappBot.Adapter.Registry's
    -- ride-index TTL floor. Both only ever read inside functions already
    -- scoped to one merchant's config row (buildBotEnv/buildTrackerDeps).
    sessionTtlSec :: Int,
    trackerMaxAgeSec :: Int,
    -- Tracker sweep interval, per merchant (WhatsappBot.Adapter.Tracker). The
    -- tracker loop is shared across every merchant, so it re-derives its
    -- actual sleep each tick as the minimum trackerPollMs among currently
    -- enabled rows, rather than reading one merchant's value in isolation.
    trackerPollMs :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
