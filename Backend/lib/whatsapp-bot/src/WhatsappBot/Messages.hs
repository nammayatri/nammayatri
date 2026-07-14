-- | Shared flexi message builders + ride-stage classifier — port of
-- @ny-connectors/connectors/src/flow/flexi-messages.ts@. Pure: each builder
-- takes a flattened 'BotBookingDetails' + a language and returns text (+ buttons)
-- to send. Used by BOTH the engine's in-handler driver poll and the background
-- tracker, so the driver card / progress messages have one source of truth.
module WhatsappBot.Messages
  ( BuiltMessage (..),
    RideStage (..),
    stageKey,
    classifyStage,
    formatDialable,
    buildDriverCard,
    buildArrived,
    buildStarted,
    buildEnded,
    buildCancelled,
  )
where

import Data.Char (isDigit)
import qualified Data.Text as T
import Kernel.Prelude
import WhatsappBot.I18n (SupportedLanguage, t)
-- Instances only: LanguageStrings is dot-accessed via RDP (getField), so its
-- name/selectors are never referenced directly, but the HasField instances are
-- needed. (Importing the names triggers -Wunused-imports under -Werror.)
import WhatsappBot.I18n.Types ()
import WhatsappBot.Types
import WhatsappBot.Util (fmtInt, fmtNum)

-- | A built outbound message (@flexi-messages.ts:12-15@). TS models buttons as a
-- 2-D array but only ever emits a single row of description-less reply buttons;
-- we flatten to @[OutButton]@ (empty = no buttons).
data BuiltMessage = BuiltMessage
  { bmText :: Text,
    bmButtons :: [OutButton]
  }
  deriving (Show, Eq, Generic)

-- | The ride's lifecycle position (@flexi-messages.ts:24-25@).
data RideStage
  = StageAssigned
  | StageArrived
  | StageStarted
  | StageCompleted
  | StageCancelled
  | StageNone
  deriving (Show, Eq, Generic)

-- | The lowercase stage name the tracker uses for its per-(booking,stage) claim
-- key and the @ORDER@ progression (@ride-tracker.ts:23@).
stageKey :: RideStage -> Text
stageKey = \case
  StageAssigned -> "assigned"
  StageArrived -> "arrived"
  StageStarted -> "started"
  StageCompleted -> "completed"
  StageCancelled -> "cancelled"
  StageNone -> "none"

-- | Classify a booking into a lifecycle stage. Order matters: terminal states
-- first, then start, arrival, bare assignment (@flexi-messages.ts:48-59@).
classifyStage :: BotBookingDetails -> RideStage
classifyStage b
  | rStatus == "CANCELLED" || bStatus == "CANCELLED" = StageCancelled
  | rStatus == "COMPLETED" || bStatus == "COMPLETED" || isJust b.rideEndTime = StageCompleted
  | rStatus == "INPROGRESS" || isJust b.rideStartTime = StageStarted
  | isJust b.driverArrivalTime = StageArrived
  | present b.driverName || present b.vehicleNumber || present b.rideOtp = StageAssigned
  | otherwise = StageNone
  where
    bStatus = T.toUpper b.bookingStatus
    rStatus = maybe "" T.toUpper b.rideStatus
    present = maybe False (not . T.null) -- TS treats an empty string as falsy/absent

-- | Normalize a phone so WhatsApp auto-linkifies it to a tap-to-dial link
-- (@flexi-messages.ts:36-44@). Bare 10-digit -> @+91@; @91XXXXXXXXXX@ -> @+…@;
-- a leading @0@ (masked exophone) is returned as-is; else prefix @+@.
formatDialable :: Maybe Text -> Maybe Text
formatDialable Nothing = Nothing
formatDialable (Just phone)
  | T.null d = Nothing
  | T.length d == 10 = Just ("+91" <> d)
  | T.length d == 12 && "91" `T.isPrefixOf` d = Just ("+" <> d)
  | "0" `T.isPrefixOf` d = Just phone
  | otherwise = Just ("+" <> d)
  where
    d = T.filter isDigit phone

-- | The "auto found — driver on the way" card (@flexi-messages.ts:62-87@).
buildDriverCard :: BotBookingDetails -> Maybe SupportedLanguage -> BuiltMessage
buildDriverCard b lang =
  BuiltMessage
    { bmText = T.intercalate "\n" msgLines,
      bmButtons = [OutButton {btnId = "cancel_confirm:" <> b.bookingId, btnTitle = s.cancelRide, btnDesc = Nothing}]
    }
  where
    s = t lang
    driverName = fromMaybe "" b.driverName
    dial = formatDialable b.driverNumber
    msgLines =
      [s.flexiFoundDriver driverName]
        <> maybe [] (\(r, e) -> [s.flexiDriverMeta (fmtNum r) (fmtInt e)]) ((,) <$> b.driverRating <*> b.etaMinutes)
        <> maybe [] (\v -> [s.vehicleLabel v]) b.vehicleNumber
        <> maybe [] (\o -> ["", s.flexiOtpShare o]) b.rideOtp
        <> maybe [] (\dl -> ["", s.flexiCallDriver dl]) dial
        <> ["", s.flexiSafetyNote]

-- | Driver reached the pickup point; re-share the start OTP if present
-- (@flexi-messages.ts:90-95@). @flexiArrived@ branches internally on empty OTP.
buildArrived :: BotBookingDetails -> Maybe SupportedLanguage -> BuiltMessage
buildArrived b lang =
  BuiltMessage {bmText = (t lang).flexiArrived (fromMaybe "" b.rideOtp), bmButtons = []}

-- | Start-OTP entered — trip underway (@flexi-messages.ts:99-110@). The End-ride
-- button is rental-only, gated on the end OTP's presence.
buildStarted :: BotBookingDetails -> Maybe SupportedLanguage -> BuiltMessage
buildStarted b lang
  | isJust b.endOtp =
    BuiltMessage
      { bmText = s.flexiRideStarted,
        bmButtons = [OutButton {btnId = "flexi_end_otp:" <> b.bookingId, btnTitle = s.flexiEndRideButton, btnDesc = Nothing}]
      }
  | otherwise = BuiltMessage {bmText = s.rideStartedSimple, bmButtons = []}
  where
    s = t lang

-- | Ride completed — surface the real final fare + distance when available
-- (@flexi-messages.ts:113-128@). meters -> km rounded to 1dp.
buildEnded :: BotBookingDetails -> Maybe SupportedLanguage -> BuiltMessage
buildEnded b lang =
  BuiltMessage
    { bmText = s.flexiRideEnded fareLine,
      bmButtons = [OutButton {btnId = "book", btnTitle = s.flexiBookAnother, btnDesc = Nothing}]
    }
  where
    s = t lang
    -- JS Math.round is round-half-UP (Math.round 12.5 == 13); Haskell `round` is
    -- banker's rounding, so use floor(x + 0.5) to match (distances are >= 0).
    km = fmap (\d -> fromIntegral (floor (d / 100 + 0.5) :: Integer) / 10 :: Double) b.chargeableDistanceM
    fareLine = case b.computedFare of
      Just f -> s.flexiFareFinal (fmtNum f) (fmtNum <$> km)
      Nothing -> s.flexiFareUnavailable

-- | Ride cancelled (@flexi-messages.ts:131-137@).
buildCancelled :: Maybe SupportedLanguage -> BuiltMessage
buildCancelled lang =
  BuiltMessage
    { bmText = (t lang).flexiRideCancelled,
      bmButtons = [OutButton {btnId = "book", btnTitle = (t lang).flexiBookAnother, btnDesc = Nothing}]
    }
