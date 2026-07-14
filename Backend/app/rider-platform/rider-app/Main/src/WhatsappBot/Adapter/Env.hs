{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Wiring hub: resolve a webhook config row to rider-app ids + a 'MerchantCtx',
-- then assemble the pure engine's 'BotEnv' (or per-merchant 'TrackerDeps') from
-- the in-process 'Flow' adapters. This is where the DB-stored @MetaBotCfg@
-- becomes the engine's @MerchantCtx@ (rideMode Text → flexi/regular bools).
module WhatsappBot.Adapter.Env
  ( dispatchInbound,
    buildTrackerDeps,
    mkMerchantCtx,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MetaWebhookConfig as DMWC
import Environment
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common (fromMaybeM, getCurrentTime, logError)
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error
import Tools.Meta (lookupMetaCfg)
import WhatsappBot.Adapter.Backend (mkBackendHandle)
import WhatsappBot.Adapter.PersonStore (mkPersonStore)
import WhatsappBot.Adapter.Registry (mkRideRegistry)
import WhatsappBot.Adapter.Sender (mkWaSender)
import WhatsappBot.Adapter.SessionStore (mkSessionStore)
import WhatsappBot.Adapter.Translations (getTranslationsMap)
import WhatsappBot.Engine (handleMessage)
import WhatsappBot.Env (BotConfig (..), BotEnv (..))
import WhatsappBot.Handles (Clock (..), RideRegistry (..))
import qualified WhatsappBot.I18n as WI
import WhatsappBot.I18n.Types (LanguageStrings, SupportedLanguage)
import WhatsappBot.Tracker (TrackerDeps (..))
import WhatsappBot.Types (InboundEvent (..), MerchantCtx (..), RideMode (..))

-- | @MetaBotCfg@ (app-env) → the engine's @MerchantCtx@. @rideMode@ is the single
-- enablement source (case-insensitive @flexi@|@regular@|@both@; anything else =>
-- neither offered), from which the two bools are DERIVED so they can't contradict
-- (config.ts:19-20).
mkMerchantCtx :: DMWC.MetaBotCfg -> MerchantCtx
mkMerchantCtx c =
  let rm = T.toLower (T.strip c.rideMode)
      flexiOn = rm == "flexi" || rm == "both"
      regularOn = rm == "regular" || rm == "both"
      mode = case rm of
        "flexi" -> RideModeFlexi
        "regular" -> RideModeRegular
        _ -> RideModeBoth
   in MerchantCtx
        { merchantLabel = c.merchantLabel,
          rideMode = mode,
          flexiEnabled = flexiOn,
          regularEnabled = regularOn,
          flexiBaseFare = c.flexiBaseFare,
          flexiPerKm = c.flexiPerKm,
          flexiServiceArea = c.flexiServiceArea,
          flexiServiceRadiusKm = c.flexiServiceRadiusKm,
          flexiRentalDistanceM = c.flexiRentalDistanceM,
          flexiRentalDurationS = c.flexiRentalDurationS,
          flexiIntroVideoUrl = c.flexiIntroVideoUrl,
          flexiSupportPhone = c.flexiSupportPhone,
          nyTrackingUrl = c.nyTrackingUrl,
          flexiQuotePollAttempts = c.flexiQuotePollAttempts,
          flexiQuotePollIntervalMs = c.flexiQuotePollIntervalMs,
          regularEstimatePollAttempts = c.regularEstimatePollAttempts,
          regularEstimatePollIntervalMs = c.regularEstimatePollIntervalMs,
          driverPollAttempts = c.driverPollAttempts,
          driverPollIntervalMs = c.driverPollIntervalMs,
          driverPollNotifyEvery = c.driverPollNotifyEvery
        }

-- | Resolve the DB row's typed merchantId/merchantOperatingCityId to rider-app
-- ids + MerchantCtx. These are stored directly on MetaWebhookConfig (not
-- parsed from Text like the old Dhall shortId/city pair), so this is a cheap,
-- cached existence check rather than a lookup — still worth doing rather than
-- trusting the row blindly, so a typo'd id at INSERT time fails loudly at
-- dispatch time instead of silently misrouting.
resolveMerchant :: DMWC.MetaWebhookConfig -> Flow (Id DM.Merchant, Id DMOC.MerchantOperatingCity, MerchantCtx)
resolveMerchant cfg = do
  merchant <- QMerchant.findById cfg.merchantId >>= fromMaybeM (MerchantDoesNotExist cfg.merchantId.getId)
  moc <- CQMOC.findById cfg.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound cfg.merchantOperatingCityId.getId)
  pure (merchant.id, moc.id, mkMerchantCtx cfg.botConfig)

mkClock :: Clock Flow
mkClock = Clock {now = getCurrentTime, sleepMs = \n -> threadDelay (n * 1000)}

-- | Poll constants come from the merchant's own meta_config row (bot_config
-- JSON) via MerchantCtx — no code-level default here. TS parity values
-- (L11) live only in the DB row's seed data and in the golden test fixtures.
mkBotConfig :: MerchantCtx -> Map.Map SupportedLanguage LanguageStrings -> BotConfig
mkBotConfig ctx translations =
  BotConfig
    { merchant = ctx,
      flexiQuotePollAttempts = ctx.flexiQuotePollAttempts,
      flexiQuotePollIntervalMs = ctx.flexiQuotePollIntervalMs,
      regularEstimatePollAttempts = ctx.regularEstimatePollAttempts,
      regularEstimatePollIntervalMs = ctx.regularEstimatePollIntervalMs,
      driverPollAttempts = ctx.driverPollAttempts,
      driverPollIntervalMs = ctx.driverPollIntervalMs,
      driverPollNotifyEvery = ctx.driverPollNotifyEvery,
      translations = translations
    }

-- | Assemble the full engine environment for one inbound (used by the webhook).
buildBotEnv :: DMWC.MetaWebhookConfig -> Flow (BotEnv Flow)
buildBotEnv cfg = do
  (merchantId, mocId, ctx) <- resolveMerchant cfg
  metaCfg <- lookupMetaCfg cfg
  let sessTtl = cfg.botConfig.sessionTtlSec
      trackMaxAge = cfg.botConfig.trackerMaxAgeSec
  translations <- getTranslationsMap mocId
  pure
    BotEnv
      { backend = mkBackendHandle merchantId mocId ctx,
        sender = mkWaSender metaCfg,
        sessions = mkSessionStore sessTtl,
        persons = mkPersonStore,
        registry = mkRideRegistry trackMaxAge,
        clock = mkClock,
        cfg = mkBotConfig ctx translations
      }

-- | Run one inbound message through the golden-tested engine with the prod env.
-- The env build (merchant/city resolve + MetaCfg) is separated from the engine
-- run: an env-build failure only logs (there is no sender to reply with), while an
-- engine crash still replies @somethingWentWrong@ via the built sender (which never
-- throws). This is the fork body — all heavy work already runs off the webhook ack.
dispatchInbound :: DMWC.MetaWebhookConfig -> InboundEvent -> Flow ()
dispatchInbound cfg ev = do
  eEnv <- try @_ @SomeException (buildBotEnv cfg)
  case eEnv of
    Left e -> logError $ "whatsapp dispatch: env build failed for " <> cfg.botConfig.merchantLabel <> ": " <> show e
    Right botEnv ->
      handleMessage botEnv ev
        `catch` \(e :: SomeException) -> do
          logError $ "whatsapp dispatch: engine crashed: " <> show e
          let msg = (WI.t botEnv.cfg.translations Nothing).somethingWentWrong
          void $ botEnv.sender.sendText ev.fromPhone msg

-- | Per-merchant tracker deps: the registry's @listRides@ is scoped to THIS
-- merchant's rides by @merchantLabel@ (claim/remove stay global by bookingId), and
-- the sender/backend are this merchant's. Correct for the 1-merchant pilot.
-- MULTI-MERCHANT CAVEAT (L6, deferred): a ride is matched to its merchant by
-- @merchantLabel@ ONLY, so merchantLabels MUST be globally unique across all
-- enabled @meta_config@ rows (this constraint became MORE important,
-- not less, now that multiple Meta Apps/rows are the whole point — not
-- enforced anywhere yet) — a collision would let one merchant's tick push
-- another merchant's ride from the wrong WhatsApp number. The multi-tenant
-- hardening (persist phone_number_id on RegisteredRide + resolve the sender
-- per-ride, as TS does) is documented, not built.
buildTrackerDeps :: DMWC.MetaWebhookConfig -> Flow (TrackerDeps Flow)
buildTrackerDeps cfg = do
  (merchantId, mocId, ctx) <- resolveMerchant cfg
  metaCfg <- lookupMetaCfg cfg
  let sessTtl = cfg.botConfig.sessionTtlSec
      trackMaxAge = cfg.botConfig.trackerMaxAgeSec
  translations <- getTranslationsMap mocId
  let fullRegistry = mkRideRegistry trackMaxAge
      scopedRegistry = fullRegistry {listRides = filter (\r -> r.merchantLabel == ctx.merchantLabel) <$> fullRegistry.listRides}
      backend = mkBackendHandle merchantId mocId ctx
  pure
    TrackerDeps
      { tdRegistry = scopedRegistry,
        tdGetBookingDetails = backend.getBookingDetails,
        tdSender = mkWaSender metaCfg,
        tdSessions = mkSessionStore sessTtl,
        tdClock = mkClock,
        tdTranslations = translations
      }
