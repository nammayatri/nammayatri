{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Assembles the pure engine's @Map SupportedLanguage LanguageStrings@ from the
-- DB-backed @atlas_app.translations@ table. A field, once wired here, is a HARD
-- requirement: a missing row throws 'WhatsappBotTranslationNotFound' rather than
-- silently falling back to the static compiled tables ('WhatsappBot.I18n.En'
-- etc.) — by design, per product decision, so a missing seed row is a loud,
-- caught-immediately data bug, not a silent stale-copy regression.
--
-- Full-migration status: EVERY field in 'LanguageStrings' is now wired here
-- (Phase 1's 6 pilot fields + batches 1-9). @static@ below is therefore only
-- ever used as the base value the record update starts from — every one of
-- its fields gets overridden, so it no longer contributes any actual copy.
-- That makes the static @WhatsappBot.I18n.{En,Hi,Gu,Kn,Ta,Te}@ modules
-- genuinely unused by this path; deleting them is the next step, once this
-- is confirmed working end-to-end (golden suite + a real multi-language
-- WhatsApp test) and the seed data (@feature-migrations/0050-...@) is
-- confirmed applied everywhere this code runs.
--
-- Runs entirely in 'Flow' (DB + cache access) — this module is the ONLY place
-- in the WhatsApp bot stack that resolves copy from the DB. Its output is a
-- plain, pre-resolved value threaded into 'BotConfig'/'TrackerDeps', so the
-- pure engine (`BotEnv m` is `Monad m =>` ONLY, see whatsapp-bot's CLAUDE.md)
-- never gains an effect.
--
-- No extra caching wraps the assembled map itself: 'LanguageStrings' has
-- function-typed fields (@Text -> Text@ substitution closures), so it cannot
-- derive 'Show'/'ToJSON' and can't go through 'Kernel.Storage.InMem.withInMemCache'
-- as a whole value. Each underlying per-key lookup is already cached (in-mem +
-- Redis, 3600s TTL) by 'Storage.CachedQueries.Translations', so re-assembling
-- the map per message/tick is just a handful of cheap in-mem cache reads once
-- warm, not fresh DB hits.
module WhatsappBot.Adapter.Translations
  ( getTranslationsMap,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment (Flow)
import Kernel.External.Types as Lang
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common (fromMaybeM)
import qualified Storage.CachedQueries.Translations as CQTranslations
import Tools.Error (WhatsappBotTranslationError (WhatsappBotTranslationNotFound))
import WhatsappBot.I18n.En (en)
import WhatsappBot.I18n.Gu (gu)
import WhatsappBot.I18n.Hi (hi)
import WhatsappBot.I18n.Kn (kn)
import WhatsappBot.I18n.Ta (ta)
import WhatsappBot.I18n.Te (te)
import WhatsappBot.I18n.Types (LanguageStrings (..), SupportedLanguage (..), allLanguages)

-- | 'SupportedLanguage' (wire codes, persisted in Redis — do not touch) →
-- @translations@ table's 'Lang.Language'. All six map 1:1.
toKernelLanguage :: SupportedLanguage -> Lang.Language
toKernelLanguage = \case
  En -> Lang.ENGLISH
  Hi -> Lang.HINDI
  Gu -> Lang.GUJARATI
  Kn -> Lang.KANNADA
  Ta -> Lang.TAMIL
  Te -> Lang.TELUGU

staticFor :: SupportedLanguage -> LanguageStrings
staticFor = \case
  En -> en
  Hi -> hi
  Gu -> gu
  Kn -> kn
  Ta -> ta
  Te -> te

-- | Positional @{{0}}@/@{{1}}@ placeholder substitution for DB-sourced templates.
substitute :: Text -> [Text] -> Text
substitute tmpl args = foldl' (\acc (i, a) -> T.replace ("{{" <> show i <> "}}") a acc) tmpl (zip [0 :: Int ..] args)

-- | The shared cached lookup's Level 3 falls back to the GLOBAL ENGLISH row
-- when the requested language has no row at all (city or global) — the right
-- behaviour for its other callers (show something rather than nothing), but
-- wrong here: a language unseeded for a given field would silently resolve to
-- our seeded English text instead of a clean "not found". Guard against it by
-- rejecting a row whose language doesn't match what was asked for — that only
-- ever happens via that Level 3 fallback, since Levels 1-2 only match on the
-- exact requested language.
lookupKey :: Id DMOC.MerchantOperatingCity -> SupportedLanguage -> Text -> Flow (Maybe Text)
lookupKey mocId lang key = do
  let wantLang = toKernelLanguage lang
  mRow <- CQTranslations.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache mocId key wantLang
  pure $ case mRow of
    Just row | row.language == wantLang -> Just row.message
    _ -> Nothing

-- | Required DB lookup for a wired field — throws 'WhatsappBotTranslationNotFound'
-- if the row is missing, rather than silently falling back to static copy. A
-- field only gets called through here once its migration seeds every language,
-- so a throw here means the seed data is missing/wrong, not "not migrated yet".
resolveField :: Id DMOC.MerchantOperatingCity -> SupportedLanguage -> Text -> Flow Text
resolveField mocId lang key =
  lookupKey mocId lang key >>= fromMaybeM (WhatsappBotTranslationNotFound key (toKernelLanguage lang))

-- | Assemble one language's 'LanguageStrings'. Wired fields are REQUIRED DB
-- reads (no fallback); everything else still comes from the static record
-- until its own batch is wired.
buildLanguageStrings :: Id DMOC.MerchantOperatingCity -> SupportedLanguage -> Flow LanguageStrings
buildLanguageStrings mocId lang = do
  let static = staticFor lang
  welcome' <- resolveField mocId lang "wa_bot_welcome"
  chooseLanguage' <- resolveField mocId lang "wa_bot_chooseLanguage"
  setupFailedTmpl <- resolveField mocId lang "wa_bot_setupFailed"
  languageUpdatedTmpl <- resolveField mocId lang "wa_bot_languageUpdated"
  flexiFareRateTmpl <- resolveField mocId lang "wa_bot_flexiFareRate"
  flexiArrivedWithOtp <- resolveField mocId lang "wa_bot_flexiArrived_withOtp"
  flexiArrivedNoOtp <- resolveField mocId lang "wa_bot_flexiArrived_noOtp"
  -- Batch 1: auth/registration fields (i18n full-migration follow-up).
  welcomeBackTmpl <- resolveField mocId lang "wa_bot_welcomeBack"
  personNotFound' <- resolveField mocId lang "wa_bot_personNotFound"
  otpSent' <- resolveField mocId lang "wa_bot_otpSent"
  invalidOtp' <- resolveField mocId lang "wa_bot_invalidOtp"
  resendOtp' <- resolveField mocId lang "wa_bot_resendOtp"
  otpResent' <- resolveField mocId lang "wa_bot_otpResent"
  otpResendFailedTmpl <- resolveField mocId lang "wa_bot_otpResendFailed"
  otpVerified' <- resolveField mocId lang "wa_bot_otpVerified"
  otpVerifyFailedTmpl <- resolveField mocId lang "wa_bot_otpVerifyFailed"
  -- Batch 2: menu/navigation fields (i18n full-migration follow-up).
  languageName' <- resolveField mocId lang "wa_bot_languageName"
  nativeLanguageName' <- resolveField mocId lang "wa_bot_nativeLanguageName"
  bookARide' <- resolveField mocId lang "wa_bot_bookARide"
  trackRide' <- resolveField mocId lang "wa_bot_trackRide"
  selectLanguage' <- resolveField mocId lang "wa_bot_selectLanguage"
  moreLanguages' <- resolveField mocId lang "wa_bot_moreLanguages"
  mainMenu' <- resolveField mocId lang "wa_bot_mainMenu"
  whatToDo' <- resolveField mocId lang "wa_bot_whatToDo"
  -- Batch 3: flexi booking flow fields (i18n full-migration follow-up).
  flexiSharePrompt' <- resolveField mocId lang "wa_bot_flexiSharePrompt"
  flexiConfirmPickupTmpl <- resolveField mocId lang "wa_bot_flexiConfirmPickup"
  flexiConfirmSavedPlaceTmpl <- resolveField mocId lang "wa_bot_flexiConfirmSavedPlace"
  pickupConfirmButton' <- resolveField mocId lang "wa_bot_pickupConfirmButton"
  pickupAdjustButton' <- resolveField mocId lang "wa_bot_pickupAdjustButton"
  flexiFinding' <- resolveField mocId lang "wa_bot_flexiFinding"
  flexiStillFindingTmpl <- resolveField mocId lang "wa_bot_flexiStillFinding"
  flexiCancelSearch' <- resolveField mocId lang "wa_bot_flexiCancelSearch"
  flexiFoundDriverTmpl <- resolveField mocId lang "wa_bot_flexiFoundDriver"
  flexiDriverMetaTmpl <- resolveField mocId lang "wa_bot_flexiDriverMeta"
  flexiOtpShareTmpl <- resolveField mocId lang "wa_bot_flexiOtpShare"
  flexiCallDriverTmpl <- resolveField mocId lang "wa_bot_flexiCallDriver"
  flexiSafetyNote' <- resolveField mocId lang "wa_bot_flexiSafetyNote"
  flexiNoAuto' <- resolveField mocId lang "wa_bot_flexiNoAuto"
  flexiTryAgain' <- resolveField mocId lang "wa_bot_flexiTryAgain"
  flexiOutOfAreaTmpl <- resolveField mocId lang "wa_bot_flexiOutOfArea"
  -- Batch 4: ride tracking/driver info fields (i18n full-migration follow-up).
  noPlacesFound' <- resolveField mocId lang "wa_bot_noPlacesFound"
  track' <- resolveField mocId lang "wa_bot_track"
  callDriver' <- resolveField mocId lang "wa_bot_callDriver"
  cancelRide' <- resolveField mocId lang "wa_bot_cancelRide"
  driverLabelTmpl <- resolveField mocId lang "wa_bot_driverLabel"
  vehicleLabelTmpl <- resolveField mocId lang "wa_bot_vehicleLabel"
  phoneLabelTmpl <- resolveField mocId lang "wa_bot_phoneLabel"
  otpLabelTmpl <- resolveField mocId lang "wa_bot_otpLabel"
  driverPhoneTmpl <- resolveField mocId lang "wa_bot_driverPhone"
  driverDetailsNotAvailable' <- resolveField mocId lang "wa_bot_driverDetailsNotAvailable"
  noActiveRide' <- resolveField mocId lang "wa_bot_noActiveRide"
  activeRide' <- resolveField mocId lang "wa_bot_activeRide"
  noActiveRidesBook' <- resolveField mocId lang "wa_bot_noActiveRidesBook"
  rideNotStarted' <- resolveField mocId lang "wa_bot_rideNotStarted"
  rideInProgressStatus' <- resolveField mocId lang "wa_bot_rideInProgressStatus"
  -- Batch 5: cancel flow fields (i18n full-migration follow-up).
  cancelConfirm' <- resolveField mocId lang "wa_bot_cancelConfirm"
  cancelConfirmWithDriverYes <- resolveField mocId lang "wa_bot_cancelConfirmWithDriver_withVehicle"
  cancelConfirmWithDriverNo <- resolveField mocId lang "wa_bot_cancelConfirmWithDriver_noVehicle"
  yesCancelIt' <- resolveField mocId lang "wa_bot_yesCancelIt"
  noKeepIt' <- resolveField mocId lang "wa_bot_noKeepIt"
  rideCancelled' <- resolveField mocId lang "wa_bot_rideCancelled"
  rideCompleted' <- resolveField mocId lang "wa_bot_rideCompleted"
  rideAlreadyCancelled' <- resolveField mocId lang "wa_bot_rideAlreadyCancelled"
  rideInProgress' <- resolveField mocId lang "wa_bot_rideInProgress"
  cancelFailedTmpl <- resolveField mocId lang "wa_bot_cancelFailed"
  cancelled' <- resolveField mocId lang "wa_bot_cancelled"
  -- Batch 6: SOS/safety fields (i18n full-migration follow-up).
  sosButton' <- resolveField mocId lang "wa_bot_sosButton"
  call112Button' <- resolveField mocId lang "wa_bot_call112Button"
  sosConfirm' <- resolveField mocId lang "wa_bot_sosConfirm"
  yesTriggerSOS' <- resolveField mocId lang "wa_bot_yesTriggerSOS"
  noGoBack' <- resolveField mocId lang "wa_bot_noGoBack"
  sosTriggered' <- resolveField mocId lang "wa_bot_sosTriggered"
  sosFailedTmpl <- resolveField mocId lang "wa_bot_sosFailed"
  markSafeButton' <- resolveField mocId lang "wa_bot_markSafeButton"
  markSafeConfirm' <- resolveField mocId lang "wa_bot_markSafeConfirm"
  yesMarkSafe' <- resolveField mocId lang "wa_bot_yesMarkSafe"
  markedSafe' <- resolveField mocId lang "wa_bot_markedSafe"
  markSafeFailedTmpl <- resolveField mocId lang "wa_bot_markSafeFailed"
  -- Batch 7: flexi ride-progress + end-ride fields (i18n full-migration follow-up).
  flexiRideStarted' <- resolveField mocId lang "wa_bot_flexiRideStarted"
  flexiFareFinalWithKm <- resolveField mocId lang "wa_bot_flexiFareFinal_withKm"
  flexiFareFinalNoKm <- resolveField mocId lang "wa_bot_flexiFareFinal_noKm"
  flexiFareUnavailable' <- resolveField mocId lang "wa_bot_flexiFareUnavailable"
  flexiRideEndedTmpl <- resolveField mocId lang "wa_bot_flexiRideEnded"
  flexiRideCancelled' <- resolveField mocId lang "wa_bot_flexiRideCancelled"
  flexiBookAnother' <- resolveField mocId lang "wa_bot_flexiBookAnother"
  flexiEndRideButton' <- resolveField mocId lang "wa_bot_flexiEndRideButton"
  flexiEndOtpShareTmpl <- resolveField mocId lang "wa_bot_flexiEndOtpShare"
  flexiEndOtpNotReady' <- resolveField mocId lang "wa_bot_flexiEndOtpNotReady"
  flexiEndOtpFetchError' <- resolveField mocId lang "wa_bot_flexiEndOtpFetchError"
  flexiRideAlreadyEnded' <- resolveField mocId lang "wa_bot_flexiRideAlreadyEnded"
  -- Batch 8: help/support/ride-type menu fields (i18n full-migration follow-up).
  moreButton' <- resolveField mocId lang "wa_bot_moreButton"
  moreTitle' <- resolveField mocId lang "wa_bot_moreTitle"
  howItWorks' <- resolveField mocId lang "wa_bot_howItWorks"
  contactSupport' <- resolveField mocId lang "wa_bot_contactSupport"
  howItWorksText' <- resolveField mocId lang "wa_bot_howItWorksText"
  howItWorksCaption' <- resolveField mocId lang "wa_bot_howItWorksCaption"
  supportMessageTmpl <- resolveField mocId lang "wa_bot_supportMessage"
  rideTypePrompt' <- resolveField mocId lang "wa_bot_rideTypePrompt"
  rideTypeFlexi' <- resolveField mocId lang "wa_bot_rideTypeFlexi"
  rideTypeRegular' <- resolveField mocId lang "wa_bot_rideTypeRegular"
  rideStartedSimple' <- resolveField mocId lang "wa_bot_rideStartedSimple"
  -- Batch 9: regular ride flow + errors (i18n full-migration follow-up).
  regularDropPrompt' <- resolveField mocId lang "wa_bot_regularDropPrompt"
  regularSelectDrop' <- resolveField mocId lang "wa_bot_regularSelectDrop"
  regularFareConfirmTmpl <- resolveField mocId lang "wa_bot_regularFareConfirm"
  regularConfirmButton' <- resolveField mocId lang "wa_bot_regularConfirmButton"
  regularChangeDropButton' <- resolveField mocId lang "wa_bot_regularChangeDropButton"
  regularSearching' <- resolveField mocId lang "wa_bot_regularSearching"
  regularBooking' <- resolveField mocId lang "wa_bot_regularBooking"
  somethingWentWrong' <- resolveField mocId lang "wa_bot_somethingWentWrong"
  sessionExpired' <- resolveField mocId lang "wa_bot_sessionExpired"
  errorTmpl <- resolveField mocId lang "wa_bot_error"
  pure
    static
      { welcome = welcome',
        chooseLanguage = chooseLanguage',
        setupFailed = \e -> substitute setupFailedTmpl [e],
        languageUpdated = \l -> substitute languageUpdatedTmpl [l],
        flexiFareRate = \base perKm -> substitute flexiFareRateTmpl [base, perKm],
        flexiArrived = \otp -> if T.null otp then flexiArrivedNoOtp else substitute flexiArrivedWithOtp [otp],
        welcomeBack = \name -> substitute welcomeBackTmpl [name],
        personNotFound = personNotFound',
        otpSent = otpSent',
        invalidOtp = invalidOtp',
        resendOtp = resendOtp',
        otpResent = otpResent',
        otpResendFailed = \e -> substitute otpResendFailedTmpl [e],
        otpVerified = otpVerified',
        otpVerifyFailed = \e -> substitute otpVerifyFailedTmpl [e],
        languageName = languageName',
        nativeLanguageName = nativeLanguageName',
        bookARide = bookARide',
        trackRide = trackRide',
        selectLanguage = selectLanguage',
        moreLanguages = moreLanguages',
        mainMenu = mainMenu',
        whatToDo = whatToDo',
        flexiSharePrompt = flexiSharePrompt',
        flexiConfirmPickup = \addr -> substitute flexiConfirmPickupTmpl [addr],
        flexiConfirmSavedPlace = \addr -> substitute flexiConfirmSavedPlaceTmpl [addr],
        pickupConfirmButton = pickupConfirmButton',
        pickupAdjustButton = pickupAdjustButton',
        flexiFinding = flexiFinding',
        flexiStillFinding = \elapsed -> substitute flexiStillFindingTmpl [elapsed],
        flexiCancelSearch = flexiCancelSearch',
        flexiFoundDriver = \name -> substitute flexiFoundDriverTmpl [name],
        flexiDriverMeta = \rating etaMin -> substitute flexiDriverMetaTmpl [rating, etaMin],
        flexiOtpShare = \otp -> substitute flexiOtpShareTmpl [otp],
        flexiCallDriver = \phone -> substitute flexiCallDriverTmpl [phone],
        flexiSafetyNote = flexiSafetyNote',
        flexiNoAuto = flexiNoAuto',
        flexiTryAgain = flexiTryAgain',
        flexiOutOfArea = \area -> substitute flexiOutOfAreaTmpl [area],
        noPlacesFound = noPlacesFound',
        track = track',
        callDriver = callDriver',
        cancelRide = cancelRide',
        driverLabel = \name -> substitute driverLabelTmpl [name],
        vehicleLabel = \number -> substitute vehicleLabelTmpl [number],
        phoneLabel = \phone -> substitute phoneLabelTmpl [phone],
        otpLabel = \otp -> substitute otpLabelTmpl [otp],
        driverPhone = \phone -> substitute driverPhoneTmpl [phone],
        driverDetailsNotAvailable = driverDetailsNotAvailable',
        noActiveRide = noActiveRide',
        activeRide = activeRide',
        noActiveRidesBook = noActiveRidesBook',
        rideNotStarted = rideNotStarted',
        rideInProgressStatus = rideInProgressStatus',
        cancelConfirm = cancelConfirm',
        cancelConfirmWithDriver = \fare mVehicle -> case mVehicle of
          Just v -> substitute cancelConfirmWithDriverYes [fare, v]
          Nothing -> substitute cancelConfirmWithDriverNo [fare],
        yesCancelIt = yesCancelIt',
        noKeepIt = noKeepIt',
        rideCancelled = rideCancelled',
        rideCompleted = rideCompleted',
        rideAlreadyCancelled = rideAlreadyCancelled',
        rideInProgress = rideInProgress',
        cancelFailed = \e -> substitute cancelFailedTmpl [e],
        cancelled = cancelled',
        sosButton = sosButton',
        call112Button = call112Button',
        sosConfirm = sosConfirm',
        yesTriggerSOS = yesTriggerSOS',
        noGoBack = noGoBack',
        sosTriggered = sosTriggered',
        sosFailed = \e -> substitute sosFailedTmpl [e],
        markSafeButton = markSafeButton',
        markSafeConfirm = markSafeConfirm',
        yesMarkSafe = yesMarkSafe',
        markedSafe = markedSafe',
        markSafeFailed = \e -> substitute markSafeFailedTmpl [e],
        flexiRideStarted = flexiRideStarted',
        flexiFareFinal = \amount mKm -> case mKm of
          Just km -> substitute flexiFareFinalWithKm [amount, km]
          Nothing -> substitute flexiFareFinalNoKm [amount],
        flexiFareUnavailable = flexiFareUnavailable',
        flexiRideEnded = \fareLine -> substitute flexiRideEndedTmpl [fareLine],
        flexiRideCancelled = flexiRideCancelled',
        flexiBookAnother = flexiBookAnother',
        flexiEndRideButton = flexiEndRideButton',
        flexiEndOtpShare = \otp -> substitute flexiEndOtpShareTmpl [otp],
        flexiEndOtpNotReady = flexiEndOtpNotReady',
        flexiEndOtpFetchError = flexiEndOtpFetchError',
        flexiRideAlreadyEnded = flexiRideAlreadyEnded',
        moreButton = moreButton',
        moreTitle = moreTitle',
        howItWorks = howItWorks',
        contactSupport = contactSupport',
        howItWorksText = howItWorksText',
        howItWorksCaption = howItWorksCaption',
        supportMessage = \name -> substitute supportMessageTmpl [name],
        rideTypePrompt = rideTypePrompt',
        rideTypeFlexi = rideTypeFlexi',
        rideTypeRegular = rideTypeRegular',
        rideStartedSimple = rideStartedSimple',
        regularDropPrompt = regularDropPrompt',
        regularSelectDrop = regularSelectDrop',
        regularFareConfirm = \fare area -> substitute regularFareConfirmTmpl [fare, area],
        regularConfirmButton = regularConfirmButton',
        regularChangeDropButton = regularChangeDropButton',
        regularSearching = regularSearching',
        regularBooking = regularBooking',
        somethingWentWrong = somethingWentWrong',
        sessionExpired = sessionExpired',
        error = \e -> substitute errorTmpl [e]
      }

-- | Assemble all 6 languages' 'LanguageStrings'. Each field resolution above is
-- a (cached) per-key DB lookup; see the module-level note on why this isn't
-- wrapped in a further whole-map cache.
getTranslationsMap :: Id DMOC.MerchantOperatingCity -> Flow (Map.Map SupportedLanguage LanguageStrings)
getTranslationsMap mocId = Map.fromList <$> mapM (\l -> (l,) <$> buildLanguageStrings mocId l) allLanguages
