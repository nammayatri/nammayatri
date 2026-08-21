-- | Shared "recipe" for assembling one language's full 'LanguageStrings':
-- which field needs which @wa_bot_*@ key(s), and how the fetched text gets
-- turned into the field's actual type (plain 'Text', or a function with
-- @{{0}}@/@{{1}}@-style blanks filled in).
--
-- Deliberately generic over HOW a key's text is fetched ('Monad m =>
-- (Text -> m Text)'), instead of being hardwired to a DB call, so this exact
-- same logic can be reused by two different callers with two different data
-- sources:
--
--   * Production: @WhatsappBot.Adapter.Translations@ (rider-app), fetching
--     each key from the real @atlas_app.translations@ DB table.
--   * Tests: @StaticI18nData@ (this package's test suite), fetching each key
--     from a frozen JSON snapshot of the same DB rows — the pure engine (and
--     its golden-replay test harness) is never allowed to touch a real DB,
--     see this package's own CLAUDE.md.
--
-- Having ONE place this mapping is written down means the two paths can
-- never quietly drift apart from each other.
module WhatsappBot.I18n.Build
  ( buildLanguageStringsM,
    substitute,
  )
where

import qualified Data.Text as T
import Kernel.Prelude hiding (error)
import WhatsappBot.I18n.Types (LanguageStrings (..))

-- | Positional @{{0}}@/@{{1}}@ placeholder substitution for DB-sourced templates.
substitute :: Text -> [Text] -> Text
substitute tmpl args = foldl' (\acc (i, a) -> T.replace ("{{" <> show i <> "}}") a acc) tmpl (zip [0 :: Int ..] args)

-- | Assemble one language's full 'LanguageStrings' by fetching each @wa_bot_*@
-- key via the supplied action. A full explicit record construction (not a
-- record-update on a base value) — every field is listed, so GHC's
-- @-Wmissing-fields@ (a hard error under @-Werror@) is the proof that nothing
-- was missed, rather than a comment claiming so.
buildLanguageStringsM :: Monad m => (Text -> m Text) -> m LanguageStrings
buildLanguageStringsM resolveField = do
  welcome' <- resolveField "wa_bot_welcome"
  chooseLanguage' <- resolveField "wa_bot_chooseLanguage"
  setupFailedTmpl <- resolveField "wa_bot_setupFailed"
  languageUpdatedTmpl <- resolveField "wa_bot_languageUpdated"
  flexiFareRateTmpl <- resolveField "wa_bot_flexiFareRate"
  flexiArrivedWithOtp <- resolveField "wa_bot_flexiArrived_withOtp"
  flexiArrivedNoOtp <- resolveField "wa_bot_flexiArrived_noOtp"
  welcomeBackTmpl <- resolveField "wa_bot_welcomeBack"
  personNotFound' <- resolveField "wa_bot_personNotFound"
  otpSent' <- resolveField "wa_bot_otpSent"
  invalidOtp' <- resolveField "wa_bot_invalidOtp"
  resendOtp' <- resolveField "wa_bot_resendOtp"
  otpResent' <- resolveField "wa_bot_otpResent"
  otpResendFailedTmpl <- resolveField "wa_bot_otpResendFailed"
  otpVerified' <- resolveField "wa_bot_otpVerified"
  otpVerifyFailedTmpl <- resolveField "wa_bot_otpVerifyFailed"
  languageName' <- resolveField "wa_bot_languageName"
  nativeLanguageName' <- resolveField "wa_bot_nativeLanguageName"
  bookARide' <- resolveField "wa_bot_bookARide"
  trackRide' <- resolveField "wa_bot_trackRide"
  selectLanguage' <- resolveField "wa_bot_selectLanguage"
  moreLanguages' <- resolveField "wa_bot_moreLanguages"
  mainMenu' <- resolveField "wa_bot_mainMenu"
  whatToDo' <- resolveField "wa_bot_whatToDo"
  flexiSharePrompt' <- resolveField "wa_bot_flexiSharePrompt"
  flexiConfirmPickupTmpl <- resolveField "wa_bot_flexiConfirmPickup"
  flexiConfirmSavedPlaceTmpl <- resolveField "wa_bot_flexiConfirmSavedPlace"
  pickupConfirmButton' <- resolveField "wa_bot_pickupConfirmButton"
  pickupAdjustButton' <- resolveField "wa_bot_pickupAdjustButton"
  flexiFinding' <- resolveField "wa_bot_flexiFinding"
  flexiStillFindingTmpl <- resolveField "wa_bot_flexiStillFinding"
  flexiCancelSearch' <- resolveField "wa_bot_flexiCancelSearch"
  flexiFoundDriverTmpl <- resolveField "wa_bot_flexiFoundDriver"
  flexiDriverMetaTmpl <- resolveField "wa_bot_flexiDriverMeta"
  flexiOtpShareTmpl <- resolveField "wa_bot_flexiOtpShare"
  flexiCallDriverTmpl <- resolveField "wa_bot_flexiCallDriver"
  flexiSafetyNote' <- resolveField "wa_bot_flexiSafetyNote"
  flexiNoAuto' <- resolveField "wa_bot_flexiNoAuto"
  flexiTryAgain' <- resolveField "wa_bot_flexiTryAgain"
  flexiOutOfAreaTmpl <- resolveField "wa_bot_flexiOutOfArea"
  noPlacesFound' <- resolveField "wa_bot_noPlacesFound"
  track' <- resolveField "wa_bot_track"
  callDriver' <- resolveField "wa_bot_callDriver"
  cancelRide' <- resolveField "wa_bot_cancelRide"
  driverLabelTmpl <- resolveField "wa_bot_driverLabel"
  vehicleLabelTmpl <- resolveField "wa_bot_vehicleLabel"
  phoneLabelTmpl <- resolveField "wa_bot_phoneLabel"
  otpLabelTmpl <- resolveField "wa_bot_otpLabel"
  driverPhoneTmpl <- resolveField "wa_bot_driverPhone"
  driverDetailsNotAvailable' <- resolveField "wa_bot_driverDetailsNotAvailable"
  noActiveRide' <- resolveField "wa_bot_noActiveRide"
  activeRide' <- resolveField "wa_bot_activeRide"
  noActiveRidesBook' <- resolveField "wa_bot_noActiveRidesBook"
  rideNotStarted' <- resolveField "wa_bot_rideNotStarted"
  rideInProgressStatus' <- resolveField "wa_bot_rideInProgressStatus"
  cancelConfirm' <- resolveField "wa_bot_cancelConfirm"
  cancelConfirmWithDriverYes <- resolveField "wa_bot_cancelConfirmWithDriver_withVehicle"
  cancelConfirmWithDriverNo <- resolveField "wa_bot_cancelConfirmWithDriver_noVehicle"
  yesCancelIt' <- resolveField "wa_bot_yesCancelIt"
  noKeepIt' <- resolveField "wa_bot_noKeepIt"
  rideCancelled' <- resolveField "wa_bot_rideCancelled"
  rideCompleted' <- resolveField "wa_bot_rideCompleted"
  rideAlreadyCancelled' <- resolveField "wa_bot_rideAlreadyCancelled"
  rideInProgress' <- resolveField "wa_bot_rideInProgress"
  cancelFailedTmpl <- resolveField "wa_bot_cancelFailed"
  cancelled' <- resolveField "wa_bot_cancelled"
  sosButton' <- resolveField "wa_bot_sosButton"
  call112Button' <- resolveField "wa_bot_call112Button"
  sosConfirm' <- resolveField "wa_bot_sosConfirm"
  yesTriggerSOS' <- resolveField "wa_bot_yesTriggerSOS"
  noGoBack' <- resolveField "wa_bot_noGoBack"
  sosTriggered' <- resolveField "wa_bot_sosTriggered"
  sosFailedTmpl <- resolveField "wa_bot_sosFailed"
  markSafeButton' <- resolveField "wa_bot_markSafeButton"
  markSafeConfirm' <- resolveField "wa_bot_markSafeConfirm"
  yesMarkSafe' <- resolveField "wa_bot_yesMarkSafe"
  markedSafe' <- resolveField "wa_bot_markedSafe"
  markSafeFailedTmpl <- resolveField "wa_bot_markSafeFailed"
  flexiRideStarted' <- resolveField "wa_bot_flexiRideStarted"
  flexiFareFinalWithKm <- resolveField "wa_bot_flexiFareFinal_withKm"
  flexiFareFinalNoKm <- resolveField "wa_bot_flexiFareFinal_noKm"
  flexiFareUnavailable' <- resolveField "wa_bot_flexiFareUnavailable"
  flexiRideEndedTmpl <- resolveField "wa_bot_flexiRideEnded"
  flexiRideCancelled' <- resolveField "wa_bot_flexiRideCancelled"
  flexiBookAnother' <- resolveField "wa_bot_flexiBookAnother"
  flexiEndRideButton' <- resolveField "wa_bot_flexiEndRideButton"
  flexiEndOtpShareTmpl <- resolveField "wa_bot_flexiEndOtpShare"
  flexiEndOtpNotReady' <- resolveField "wa_bot_flexiEndOtpNotReady"
  flexiEndOtpFetchError' <- resolveField "wa_bot_flexiEndOtpFetchError"
  flexiRideAlreadyEnded' <- resolveField "wa_bot_flexiRideAlreadyEnded"
  moreButton' <- resolveField "wa_bot_moreButton"
  moreTitle' <- resolveField "wa_bot_moreTitle"
  howItWorks' <- resolveField "wa_bot_howItWorks"
  contactSupport' <- resolveField "wa_bot_contactSupport"
  howItWorksText' <- resolveField "wa_bot_howItWorksText"
  howItWorksCaption' <- resolveField "wa_bot_howItWorksCaption"
  supportMessageTmpl <- resolveField "wa_bot_supportMessage"
  rideTypePrompt' <- resolveField "wa_bot_rideTypePrompt"
  rideTypeFlexi' <- resolveField "wa_bot_rideTypeFlexi"
  rideTypeRegular' <- resolveField "wa_bot_rideTypeRegular"
  rideStartedSimple' <- resolveField "wa_bot_rideStartedSimple"
  regularDropPrompt' <- resolveField "wa_bot_regularDropPrompt"
  regularSelectDrop' <- resolveField "wa_bot_regularSelectDrop"
  regularFareConfirmTmpl <- resolveField "wa_bot_regularFareConfirm"
  regularConfirmButton' <- resolveField "wa_bot_regularConfirmButton"
  regularChangeDropButton' <- resolveField "wa_bot_regularChangeDropButton"
  regularSearching' <- resolveField "wa_bot_regularSearching"
  regularBooking' <- resolveField "wa_bot_regularBooking"
  somethingWentWrong' <- resolveField "wa_bot_somethingWentWrong"
  sessionExpired' <- resolveField "wa_bot_sessionExpired"
  errorTmpl <- resolveField "wa_bot_error"
  pure
    LanguageStrings
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
