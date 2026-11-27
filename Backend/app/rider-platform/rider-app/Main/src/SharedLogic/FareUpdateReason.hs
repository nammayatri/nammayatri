{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The end-of-ride "Fare increased/reduced by X as the ..." sentence, built
-- on the backend so fare-logic and wording changes need no app release.
--
-- This is a line-by-line port of the consumer app's logic as it stood when
-- the backend took it over:
--   * fare difference + inputs: @consumer/src-v2/screens/reviewAndFeedback/hooks/useFareCalculation.ts@ (computeFareBreakdown)
--   * reasons + sentence:       @consumer/src-v2/screens/reviewAndFeedback/variants/classic/UI.tsx@ (fareUpdatedText)
--   * charge-line gross-up:     @consumer/src-v2/utils/tax.ts@ (grossFareLine)
-- with two deliberate fixes to the rental reasons (see 'computeFareUpdate'):
-- extra time needs a full minute, extra distance needs a longer ride; and one
-- addition the app never had: non-rental rides name extra ride time when the
-- fare policy billed it (EXTRA_TIME_FARE / RIDE_DURATION_FARE).
-- Keep it in step with the app's fallback until every app version reads
-- 'FareUpdateInfo'; after that, change the rules here only.
module SharedLogic.FareUpdateReason
  ( FareUpdateInfo (..),
    FareUpdateInput (..),
    FareUpdate (..),
    FareChangeReason (..),
    computeFareUpdate,
    renderFareUpdateText,
    mkFareUpdateInfo,
    FareUpdateText (..),
    fareUpdateTextKey,
    englishText,
    formatDistanceText,
    formatExtraTimeText,
    formatAmountText,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import qualified BecknV2.OnDemand.Settlement as Settlement
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)
import qualified Domain.Types.Booking as DBooking
import Domain.Types.FareBreakup (FareBreakupAPIEntity (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.TranslationKeys (RiderTranslationKey (..))
import qualified Kernel.External.Types as Lang
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMeters, HighPrecMoney (..), Money (..), PriceAPIEntity (..), Seconds (..), getAccuracy)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, logTagInfo)
import SharedLogic.FareBreakupInfo (tipFareBreakupTitle)
import qualified Storage.CachedQueries.Translations as CQTranslations

-- | What the app shows under the final fare. Absent when nothing changed.
data FareUpdateInfo = FareUpdateInfo
  { -- | The final sentence, in the rider's language.
    text :: Text,
    -- | Drives the up/down icon.
    isFareIncreased :: Bool,
    -- | Absolute difference, rounded to what the screen displays.
    fareDifference :: PriceAPIEntity
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- | Everything the calculation reads, taken from the same BookingAPIEntity
-- (and its rideList head) the app receives.
data FareUpdateInput = FareUpdateInput
  { finalFareBreakup :: [FareBreakupAPIEntity],
    estimatedFareBreakup :: [FareBreakupAPIEntity],
    -- | ride.componentGrossMultipliers, else booking.componentGrossMultipliers.
    grossMultipliers :: Maybe (FB.RideTaxRates Settlement.GrossMultiplier),
    -- | ride.computedPriceWithCurrency. Required: the app's fallback to
    -- booking.estimatedFareWithCurrency's currency only matters when this is
    -- absent, and then there is no final fare to compare.
    computedPrice :: Maybe PriceAPIEntity,
    -- | booking.estimatedTotalFareWithCurrency: the compared estimate, and the
    -- currency the sentence is formatted in (as on the classic screen).
    estimatedTotalFare :: PriceAPIEntity,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    estimatedDuration :: Maybe Seconds,
    estimatedDistance :: Maybe HighPrecMeters,
    chargeableDistance :: Maybe HighPrecMeters,
    -- | booking-details tag is RENTAL (EASY_BOOKING is not).
    isRental :: Bool
  }
  deriving (Generic, Show)

data FareChangeReason
  = -- | Non-rental: "ride was {dist} longer|shorter". True = longer.
    DistanceChanged Double Bool
  | -- | "ride was {dist} and {time} longer" (rental, and non-rental extra ride time).
    DistanceAndTimeLonger Double Int
  | -- | Rental: "ride was {dist} longer".
    DistanceLonger Double
  | -- | "ride was {time} longer" (rental, and non-rental extra ride time).
    TimeLonger Int
  | -- | Rental fare decrease: "ride was shorter".
    RentalRideShorter
  | -- | "wait time was longer".
    WaitTimeLonger
  deriving (Show, Eq)

data FareUpdate = FareUpdate
  { -- | Signed, display-rounded: final minus estimate.
    signedDifference :: Rational,
    reasons :: [FareChangeReason],
    displayCurrency :: Currency
  }
  deriving (Show, Eq)

-- | Nothing when the displayed fare did not change (or the ride has no final price yet).
computeFareUpdate :: FareUpdateInput -> Maybe FareUpdate
computeFareUpdate input = do
  finalPrice <- input.computedPrice
  let gross = grossFareLine input.grossMultipliers
      actualToll = gross input.finalFareBreakup tollTitle
      estimatedToll = gross input.estimatedFareBreakup tollTitle
      -- The rider's own tip is never a fare change. The estimate never holds one, so it is
      -- left out of the final fare in every city (the app only did this where fundraising
      -- was on, and elsewhere called a tip a fare increase).
      tip = (\line -> toRational line.amount.getMoney) <$> find ((== tipFareBreakupTitle) . (.description)) input.finalFareBreakup
      roundingCurrency = finalPrice.currency
      finalWithoutToll = finalPrice.amount.getHighPrecMoney - fromMaybe 0 actualToll - fromMaybe 0 tip
      estimateWithoutToll = input.estimatedTotalFare.amount.getHighPrecMoney - fromMaybe 0 estimatedToll
      diff = roundToDisplayPrecision roundingCurrency finalWithoutToll - roundToDisplayPrecision roundingCurrency estimateWithoutToll
  guard (diff /= 0)
  let increased = diff > 0
      waiting = gross input.finalFareBreakup "WAITING_OR_PICKUP_CHARGES"
      extraTimeFare = gross input.finalFareBreakup "TIME_BASED_FARE"
      extraDistanceFare = gross input.finalFareBreakup "DIST_BASED_FARE"
      -- getDiffBetweenTimes(start, end) - (estimatedDuration ?? 0); a missing
      -- time is NaN in the app, i.e. no extra time.
      mbExtraTime = do
        start <- input.rideStartTime
        end <- input.rideEndTime
        pure $ (floor (realToFrac (diffUTCTime end start) :: Double) :: Int) - maybe 0 (.getSeconds) input.estimatedDuration
      actual = maybe 0 realToFrac input.chargeableDistance :: Double
      estimated = maybe 0 realToFrac input.estimatedDistance :: Double
      distanceDiff = abs (estimated - actual)
      isLonger = actual > estimated
      hasWaiting = positive waiting
      -- Rental reasons. Two deliberate departures from the app, which checked
      -- extraTime > 0 and distanceDiff > 0:
      --   * time counts only from a full minute, so the sentence never says
      --     "ride was 0 min longer" for a few seconds' overrun;
      --   * distance counts only when the ride was actually longer, so a ride
      --     that drove less is never described as "longer".
      hasExtraTime = positive extraTimeFare && maybe False (>= 60) mbExtraTime
      hasExtraDistance = positive extraDistanceFare && isLonger
      extraTime = fromMaybe 0 mbExtraTime
      rentalReasons
        | not increased = [RentalRideShorter]
        | otherwise =
          let longerReason
                | hasExtraDistance && hasExtraTime = [DistanceAndTimeLonger distanceDiff extraTime]
                | hasExtraDistance = [DistanceLonger distanceDiff]
                | hasExtraTime = [TimeLonger extraTime]
                | otherwise = []
              waitingReason = [WaitTimeLonger | hasWaiting && not (hasExtraDistance && hasExtraTime)]
           in longerReason <> waitingReason
      -- Non-rental ride time (backend-only, the app never had it). A policy bills
      -- time overage through EXTRA_TIME_FARE (never on the estimate, so any amount
      -- is overage) or re-prices RIDE_DURATION_FARE on the actual minutes. Named
      -- only on an increase, only when one of those charges grew, and only for a
      -- full minute over a known estimate: a missing estimate would otherwise
      -- report the whole ride duration as "longer".
      rideTimeChargeGrew =
        positive (gross input.finalFareBreakup "EXTRA_TIME_FARE")
          || fromMaybe 0 (gross input.finalFareBreakup "RIDE_DURATION_FARE") > fromMaybe 0 (gross input.estimatedFareBreakup "RIDE_DURATION_FARE")
      hasRideExtraTime = increased && rideTimeChargeGrew && isJust input.estimatedDuration && maybe False (>= 60) mbExtraTime
      hasDistanceReason = distanceDiff > 0 && isLonger == increased
      nonRentalRideReason
        | hasRideExtraTime && hasDistanceReason = [DistanceAndTimeLonger distanceDiff extraTime]
        | hasRideExtraTime = [TimeLonger extraTime]
        | otherwise = [DistanceChanged distanceDiff isLonger | hasDistanceReason]
      nonRentalReasons = nonRentalRideReason <> [WaitTimeLonger | increased && hasWaiting]
  pure
    FareUpdate
      { signedDifference = diff,
        reasons = if input.isRental then rentalReasons else nonRentalReasons,
        displayCurrency = input.estimatedTotalFare.currency
      }
  where
    positive = maybe False (> 0)

-- | The texts the sentence is built from: one per translation key, so every
-- key and its English fallback are listed exactly once, without a catch-all.
data FareUpdateText
  = IncreasedByAmountAsThe
  | ReducedByAmountAsThe
  | IncreasedByAmount
  | ReducedByAmount
  | RideWas
  | Longer
  | Shorter
  | And
  | WaitTimeWasLonger
  deriving (Show, Eq, Enum, Bounded)

-- | The translation key a text is stored under.
fareUpdateTextKey :: FareUpdateText -> RiderTranslationKey
fareUpdateTextKey = \case
  IncreasedByAmountAsThe -> FareUpdate_Sentence_IncreasedByAmountAsThe
  ReducedByAmountAsThe -> FareUpdate_Sentence_ReducedByAmountAsThe
  IncreasedByAmount -> FareUpdate_Sentence_IncreasedByAmount
  ReducedByAmount -> FareUpdate_Sentence_ReducedByAmount
  RideWas -> FareUpdate_Reason_RideWas
  Longer -> FareUpdate_Reason_Longer
  Shorter -> FareUpdate_Reason_Shorter
  And -> FareUpdate_Reason_And
  WaitTimeWasLonger -> FareUpdate_Reason_WaitTimeWasLonger

-- | The app's English for each text (keys in the app's translation file:
-- FareIncreasedByAmountAsThe, FareReducedByAmountAsThe, FareIncreasedByAmount,
-- FareReducedByAmount, ridewas, longer, shorter, and, waittimewaslonger).
-- The gateway's fallback chain ends at the caller's text: this is shown when
-- neither the rider's language nor English has a row.
englishText :: FareUpdateText -> Text
englishText = \case
  IncreasedByAmountAsThe -> "Fare increased by {0} as the"
  ReducedByAmountAsThe -> "Fare reduced by {0} as the"
  IncreasedByAmount -> "Fare increased by {0}"
  ReducedByAmount -> "Fare reduced by {0}"
  RideWas -> "ride was"
  Longer -> "longer"
  Shorter -> "shorter"
  And -> "and"
  WaitTimeWasLonger -> "wait time was longer"

-- | Builds the sentence from the computed update, fetching each text it
-- needs through the given reader ('mkFareUpdateInfo' reads translations;
-- tests pass @pure . englishText@).
renderFareUpdateText :: Monad m => Lang.Language -> (FareUpdateText -> m Text) -> FareUpdate -> m Text
renderFareUpdateText language getText fareUpdate =
  if null fareUpdate.reasons
    then fillAmount <$> getText (if increased then IncreasedByAmount else ReducedByAmount)
    else do
      sentence <- getText (if increased then IncreasedByAmountAsThe else ReducedByAmountAsThe)
      joiner <- getText And
      reasonTexts <- mapM reasonText fareUpdate.reasons
      pure $ fillAmount sentence <> " " <> T.intercalate (" " <> joiner <> " ") reasonTexts
  where
    increased = fareUpdate.signedDifference > 0
    currency = fareUpdate.displayCurrency
    fillAmount = T.replace "{0}" (formatAmountText language currency (abs fareUpdate.signedDifference))
    dist = formatDistanceText language currency
    -- "ride was" + the given middle words + the closing text
    rideWas middle closing = do
      rideWasText <- getText RideWas
      closingText <- getText closing
      pure $ T.unwords ([rideWasText] <> middle <> [closingText])
    reasonText = \case
      DistanceChanged d longer -> rideWas [dist d] (if longer then Longer else Shorter)
      DistanceAndTimeLonger d t -> do
        andText <- getText And
        rideWas [dist d, andText, formatExtraTimeText t] Longer
      DistanceLonger d -> rideWas [dist d] Longer
      TimeLonger t -> rideWas [formatExtraTimeText t] Longer
      RentalRideShorter -> rideWas [] Shorter
      WaitTimeLonger -> getText WaitTimeWasLonger

-- | Computes and renders the update, reading each text in the rider's
-- language and city (the app's English when no row exists).
mkFareUpdateInfo ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DBooking.Booking ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Lang.Language ->
  FareUpdateInput ->
  m (Maybe FareUpdateInfo)
mkFareUpdateInfo bookingId merchantOpCityId mbLanguage input = do
  let mbFareUpdate = computeFareUpdate input
  -- What the calculation saw and what it decided: tells a data problem from a rule problem.
  logTagInfo "FareUpdateReason" $
    "bookingId=" <> bookingId.getId
      <> ", computedPrice="
      <> maybe "none" showPrice input.computedPrice
      <> ", estimatedTotalFare="
      <> showPrice input.estimatedTotalFare
      <> ", estimatedDistance="
      <> show input.estimatedDistance
      <> ", chargeableDistance="
      <> show input.chargeableDistance
      <> ", estimatedDuration="
      <> show input.estimatedDuration
      <> ", rideStartTime="
      <> show input.rideStartTime
      <> ", rideEndTime="
      <> show input.rideEndTime
      <> ", isRental="
      <> show input.isRental
      <> ", finalBreakup="
      <> showBreakup input.finalFareBreakup
      <> ", result="
      <> maybe "no fare change shown" showDecision mbFareUpdate
  forM mbFareUpdate $ \fareUpdate -> do
    let language = fromMaybe Lang.ENGLISH mbLanguage
        getText t = CQTranslations.findTextByKeyWithFallback merchantOpCityId (Just language) (fareUpdateTextKey t) (englishText t)
    sentence <- renderFareUpdateText language getText fareUpdate
    -- The text returned: tells a translation-row problem from a logic problem.
    logTagInfo "FareUpdateReason" $ "bookingId=" <> bookingId.getId <> ", language=" <> show language <> ", text=" <> sentence
    pure
      FareUpdateInfo
        { text = sentence,
          isFareIncreased = fareUpdate.signedDifference > 0,
          fareDifference = PriceAPIEntity {amount = HighPrecMoney (abs fareUpdate.signedDifference), currency = fareUpdate.displayCurrency}
        }
  where
    showPrice :: PriceAPIEntity -> Text
    showPrice p = show (fromRational p.amount.getHighPrecMoney :: Double) <> " " <> show p.currency
    showBreakup :: [FareBreakupAPIEntity] -> Text
    showBreakup breakup = "[" <> T.intercalate ", " (map (\l -> l.description <> "=" <> show (fromRational l.amountWithCurrency.amount.getHighPrecMoney :: Double)) breakup) <> "]"
    showDecision :: FareUpdate -> Text
    showDecision fareUpdate = "difference=" <> show (fromRational fareUpdate.signedDifference :: Double) <> " " <> show fareUpdate.displayCurrency <> ", reasons=" <> show fareUpdate.reasons

-- ---------------------------------------------------------------------------
-- Fare-line gross-up (tax.ts)

tollTitle :: Text
tollTitle = "TOLL_CHARGES"

-- | The first line with this title, grossed up by its component's
-- tax-exclusive multiplier (1 when the ride carries no multipliers).
grossFareLine :: Maybe (FB.RideTaxRates Settlement.GrossMultiplier) -> [FareBreakupAPIEntity] -> Text -> Maybe Rational
grossFareLine mbMultipliers breakup title = do
  line <- find ((== title) . (.description)) breakup
  let amount = line.amountWithCurrency.amount.getHighPrecMoney
  pure $ case breakupComponentOf title of
    Nothing -> amount
    Just component -> amount * maybe 1 (\multipliers -> (component multipliers).taxExclusive.getHighPrecMoney) mbMultipliers

breakupComponentOf :: Text -> Maybe (FB.RideTaxRates Settlement.GrossMultiplier -> Settlement.GrossMultiplier)
breakupComponentOf title
  | title `elem` taxTitles = Nothing
  | title == "TOLL_CHARGES" = Just (.toll)
  | title `elem` ["PARKING_CHARGE", "PARKING_CHARGES"] = Just (.parking)
  | title == "AIRPORT_ENTRY_FEE" = Just (.airportEntryFee)
  | title `elem` ["CANCELLATION_CHARGES", "RIDE_CANCELLATION_CHARGES"] = Just (.cancellationFee)
  | title == "PLATFORM_FEE" = Just (.platformFee)
  | title == "PAYMENT_CHARGE" = Just (.paymentCharge)
  | otherwise = Just (.rideFare)

-- | tax.ts TAX_TITLES: tax lines are shown as charged, never grossed up.
taxTitles :: [Text]
taxTitles =
  [ "RIDE_FARE_TAX",
    "RIDE_FARE_VAT",
    "RIDE_VAT",
    "TAX",
    "FIXED_GOVERNMENT_RATE",
    "RIDE_FARE_CGST",
    "RIDE_FARE_SGST",
    "RIDE_FARE_IGST",
    "TOLL_TAX",
    "TOLL_VAT",
    "TOLL_FARE_TAX",
    "TOLL_CGST",
    "TOLL_SGST",
    "TOLL_IGST",
    "PARKING_TAX",
    "PARKING_VAT",
    "PARKING_CHARGE_TAX",
    "PARKING_CGST",
    "PARKING_SGST",
    "PARKING_IGST",
    "AIRPORT_ENTRY_FEE_TAX",
    "AIRPORT_ENTRY_FEE_VAT",
    "AIRPORT_ENTRY_FEE_CGST",
    "AIRPORT_ENTRY_FEE_SGST",
    "AIRPORT_ENTRY_FEE_IGST",
    "CANCELLATION_FEE_TAX",
    "CANCELLATION_FEE_VAT",
    "RIDE_CANCELLATION_TAX",
    "CANCELLATION_TAX",
    "CANCELLATION_FEE_CGST",
    "CANCELLATION_FEE_SGST",
    "CANCELLATION_FEE_IGST",
    "PLATFORM_FEE_VAT",
    "PLATFORM_FEE_CGST",
    "PLATFORM_FEE_SGST",
    "PLATFORM_FEE_IGST",
    "CGST",
    "SGST",
    "PAYMENT_CHARGE_TAX",
    "PAYMENT_CHARGE_VAT",
    "PAYMENT_CHARGE_CGST",
    "PAYMENT_CHARGE_SGST",
    "PAYMENT_CHARGE_IGST"
  ]

-- ---------------------------------------------------------------------------
-- Rounding and formatting

-- | JS Math.round: halves round up.
jsRound :: Rational -> Integer
jsRound x = floor (x + 1 / 2)

-- | useFareCalculation.ts roundToDisplayPrecision, generalised to the
-- currency's own accuracy (INR 0, USD/EUR 2 as in the app; JOD 3 — the app
-- has no JOD and would show it as rupees).
roundToDisplayPrecision :: Currency -> Rational -> Rational
roundToDisplayPrecision currency amount =
  let scale = 10 ^ getAccuracy currency :: Integer
   in fromInteger (jsRound (amount * fromInteger scale)) / fromInteger scale

-- | The currency argument of the sentence: symbol + amount with the
-- currency's own number of decimals.
formatAmountText :: Lang.Language -> Currency -> Rational -> Text
formatAmountText language currency amount =
  currencySymbol <> formatFixed (numberLocaleFor language currency) (getAccuracy currency) amount
  where
    -- Spelled out per currency so a new one fails the build until it gets a symbol.
    currencySymbol = case currency of
      INR -> "\8377"
      EUR -> "\8364"
      USD -> "$"
      JOD -> "JD "

-- | regionalConfig.ts formatDistance(value, 'Kilometer').
formatDistanceText :: Lang.Language -> Currency -> Double -> Text
formatDistanceText language currency meters
  | meters >= 1000 = formatFixed (numberLocaleFor language currency) 1 (toRational meters / 1000) <> " km"
  | otherwise = show (jsRound (toRational meters)) <> " m"

-- | UI.tsx parsedExtraTime: hr/min are English in the app too.
formatExtraTimeText :: Int -> Text
formatExtraTimeText extraSeconds =
  let extraMinutes = extraSeconds `div` 60
      hours = extraMinutes `div` 60
      minutes = extraMinutes `mod` 60
   in if extraMinutes >= 60
        then if minutes > 0 then show hours <> " hr " <> show minutes <> " min" else show hours <> " hr"
        else show extraMinutes <> " min"

data Grouping = WesternGrouping | IndianGrouping | WesternGroupingFrom5Digits

data NumberLocale = NumberLocale
  { decimalSeparator :: Text,
    groupSeparator :: Text,
    grouping :: Grouping
  }

-- | The rider's language stands in for the phone locale the app formats with.
numberLocaleFor :: Lang.Language -> Currency -> NumberLocale
numberLocaleFor language currency = case language of
  Lang.ENGLISH -> NumberLocale "." "," (if currency == INR then IndianGrouping else WesternGrouping)
  Lang.DUTCH -> NumberLocale "," "." WesternGrouping
  Lang.GERMAN -> NumberLocale "," "." WesternGrouping
  Lang.ITALIAN -> NumberLocale "," "." WesternGrouping
  Lang.SPANISH -> NumberLocale "," "." WesternGroupingFrom5Digits
  Lang.FRENCH -> NumberLocale "," "\8239" WesternGrouping
  Lang.FINNISH -> NumberLocale "," "\160" WesternGrouping
  Lang.SWEDISH -> NumberLocale "," "\160" WesternGrouping
  Lang.RUSSIAN -> NumberLocale "," "\160" WesternGrouping
  Lang.ARABIC -> NumberLocale "." "," WesternGrouping
  Lang.SOMALI -> NumberLocale "." "," WesternGrouping
  -- Indian languages (hi-IN, ta-IN, ...): Indian grouping.
  _ -> NumberLocale "." "," IndianGrouping

-- | toLocaleString with fixed decimals; halves round away from zero (Intl halfExpand).
formatFixed :: NumberLocale -> Int -> Rational -> Text
formatFixed loc decimals value =
  let scale = 10 ^ decimals :: Integer
      scaled = floor (abs value * fromInteger scale + 1 / 2) :: Integer
      (intPart, fracPart) = scaled `divMod` scale
      sign = if value < 0 && scaled /= 0 then "-" else ""
      fracText = if decimals > 0 then loc.decimalSeparator <> T.justifyRight decimals '0' (show fracPart) else ""
   in sign <> groupDigits loc (show intPart) <> fracText

groupDigits :: NumberLocale -> Text -> Text
groupDigits loc digits = case loc.grouping of
  WesternGrouping -> T.intercalate loc.groupSeparator (chunksFromRight 3 digits)
  WesternGroupingFrom5Digits ->
    if T.length digits >= 5 then T.intercalate loc.groupSeparator (chunksFromRight 3 digits) else digits
  IndianGrouping ->
    if T.length digits <= 3
      then digits
      else
        let (rest, lastThree) = T.splitAt (T.length digits - 3) digits
         in T.intercalate loc.groupSeparator (chunksFromRight 2 rest <> [lastThree])

chunksFromRight :: Int -> Text -> [Text]
chunksFromRight n digits = reverse (map T.reverse (T.chunksOf n (T.reverse digits)))
