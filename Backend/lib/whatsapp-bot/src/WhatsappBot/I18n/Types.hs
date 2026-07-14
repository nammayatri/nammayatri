{-# LANGUAGE DeriveAnyClass #-}

-- | Port of @ny-connectors/connectors/src/i18n/types.ts@ (the @LanguageStrings@
-- interface) and @SupportedLanguage@.
--
-- Design note (divergence from TS, cosmetic only): TypeScript string functions
-- that take @number@ args (fares, ratings, elapsed seconds) are modelled here as
-- taking pre-formatted 'Text'. The engine formats numbers JS-style (via
-- @WhatsappBot.Util.fmtNum@) before calling these, so every language table's
-- functions are uniformly 'Text'-argumented and number formatting lives in one
-- place. The golden gate ignores copy; snapshot/live diffs see identical output.
module WhatsappBot.I18n.Types
  ( SupportedLanguage (..),
    allLanguages,
    languageCode,
    parseLanguage,
    LanguageStrings (..),
  )
where

-- 'error' is hidden because LanguageStrings has an @error@ field (port of
-- en.ts's @error@ key); Kernel.Prelude re-exports Universum's @error@, which
-- would clash with the generated field selector. Field access is via RDP dot
-- syntax, so we never need the prelude @error@ here.
import Kernel.Prelude hiding (error)

-- | The six supported languages (@i18n/types.ts:1@).
data SupportedLanguage = En | Hi | Gu | Kn | Ta | Te
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, Enum, Bounded)

allLanguages :: [SupportedLanguage]
allLanguages = [minBound .. maxBound]

-- | The wire/storage code, e.g. @En -> "en"@ (matches @SupportedLanguage@ string).
languageCode :: SupportedLanguage -> Text
languageCode = \case
  En -> "en"
  Hi -> "hi"
  Gu -> "gu"
  Kn -> "kn"
  Ta -> "ta"
  Te -> "te"

-- | Parse a language code; unknown -> Nothing (caller falls back to 'En').
parseLanguage :: Text -> Maybe SupportedLanguage
parseLanguage t = find ((== t) . languageCode) allLanguages

-- | The full string table each language must implement (@i18n/types.ts:3-141@).
-- Field order mirrors the TS interface verbatim. Fields that were
-- @(args) => string@ in TS become functions; numeric args are pre-formatted
-- 'Text' (see the module note). Optional TS args (@vehicle?@, @km?@) become
-- 'Maybe' 'Text', with the branching preserved inside each implementation.
data LanguageStrings = LanguageStrings
  { -- Language metadata
    languageName :: Text,
    nativeLanguageName :: Text,
    -- Welcome & menu
    bookARide :: Text,
    trackRide :: Text,
    chooseLanguage :: Text,
    selectLanguage :: Text,
    languageUpdated :: Text -> Text,
    moreLanguages :: Text,
    -- Auth flow
    setupFailed :: Text -> Text,
    -- Registration (OTP-based) — retained for copy fidelity though the port
    -- uses silent onboarding; personNotFound/otpSent still surface in flows.
    personNotFound :: Text,
    otpSent :: Text,
    invalidOtp :: Text,
    resendOtp :: Text,
    otpResent :: Text,
    otpResendFailed :: Text -> Text,
    otpVerified :: Text,
    otpVerifyFailed :: Text -> Text,
    -- Origin & destination
    noPlacesFound :: Text,
    -- Booking
    track :: Text,
    callDriver :: Text,
    cancelRide :: Text,
    driverLabel :: Text -> Text,
    vehicleLabel :: Text -> Text,
    phoneLabel :: Text -> Text,
    otpLabel :: Text -> Text,
    driverPhone :: Text -> Text,
    driverDetailsNotAvailable :: Text,
    noActiveRide :: Text,
    -- No driver found
    mainMenu :: Text,
    -- Status
    activeRide :: Text,
    noActiveRidesBook :: Text,
    -- Cancel
    cancelConfirm :: Text,
    cancelConfirmWithDriver :: Text -> Maybe Text -> Text,
    yesCancelIt :: Text,
    noKeepIt :: Text,
    rideCancelled :: Text,
    rideCompleted :: Text,
    rideAlreadyCancelled :: Text,
    rideInProgress :: Text,
    cancelFailed :: Text -> Text,
    cancelled :: Text,
    whatToDo :: Text,
    -- SOS & safety
    rideNotStarted :: Text,
    rideInProgressStatus :: Text,
    sosButton :: Text,
    call112Button :: Text,
    sosConfirm :: Text,
    yesTriggerSOS :: Text,
    noGoBack :: Text,
    sosTriggered :: Text,
    sosFailed :: Text -> Text,
    markSafeButton :: Text,
    markSafeConfirm :: Text,
    yesMarkSafe :: Text,
    markedSafe :: Text,
    markSafeFailed :: Text -> Text,
    -- Flexi (location-only metered booking)
    welcome :: Text,
    flexiSharePrompt :: Text,
    flexiFareRate :: Text -> Text -> Text, -- (base, perKm) pre-formatted
    flexiConfirmPickup :: Text -> Text,
    flexiConfirmSavedPlace :: Text -> Text,
    pickupConfirmButton :: Text,
    pickupAdjustButton :: Text,
    flexiFinding :: Text,
    flexiStillFinding :: Text -> Text, -- (elapsed) pre-formatted
    flexiCancelSearch :: Text,
    flexiFoundDriver :: Text -> Text,
    flexiDriverMeta :: Text -> Text -> Text, -- (rating, etaMin) pre-formatted
    flexiOtpShare :: Text -> Text,
    flexiCallDriver :: Text -> Text,
    flexiSafetyNote :: Text,
    flexiNoAuto :: Text,
    flexiTryAgain :: Text,
    flexiOutOfArea :: Text -> Text,
    -- Flexi ride-progress updates (pushed by the background tracker)
    flexiArrived :: Text -> Text, -- (otp) branches on empty
    flexiRideStarted :: Text,
    flexiFareFinal :: Text -> Maybe Text -> Text, -- (amount, km?) pre-formatted
    flexiFareUnavailable :: Text,
    flexiRideEnded :: Text -> Text, -- (fareLine)
    flexiRideCancelled :: Text,
    flexiBookAnother :: Text,
    -- Flexi end-ride OTP (rental)
    flexiEndRideButton :: Text,
    flexiEndOtpShare :: Text -> Text,
    flexiEndOtpNotReady :: Text,
    flexiEndOtpFetchError :: Text,
    flexiRideAlreadyEnded :: Text,
    -- Flexi "hi" menu — More drawer + how-it-works + support
    moreButton :: Text,
    moreTitle :: Text,
    howItWorks :: Text,
    contactSupport :: Text,
    howItWorksText :: Text,
    howItWorksCaption :: Text,
    supportMessage :: Text -> Text,
    -- Ride-type chooser + generic ride-started
    rideTypePrompt :: Text,
    rideTypeFlexi :: Text,
    rideTypeRegular :: Text,
    rideStartedSimple :: Text,
    -- Regular one-way flow (pickup + drop -> auto fare -> book)
    regularDropPrompt :: Text,
    regularSelectDrop :: Text,
    regularFareConfirm :: Text -> Text -> Text, -- (fare, area) fare pre-formatted
    regularConfirmButton :: Text,
    regularChangeDropButton :: Text,
    regularSearching :: Text,
    regularBooking :: Text,
    -- Errors
    somethingWentWrong :: Text,
    sessionExpired :: Text,
    error :: Text -> Text
  }
