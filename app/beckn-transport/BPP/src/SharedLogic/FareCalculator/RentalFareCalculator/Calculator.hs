{-# LANGUAGE DerivingStrategies #-}

module SharedLogic.FareCalculator.RentalFareCalculator.Calculator where

import Beckn.Types.Amount (Amount (..))
import Beckn.Types.Common
import Data.Time
  ( LocalTime (localDay),
    TimeZone,
    diffDays,
    minutesToTimeZone,
    utcToLocalTime,
  )
import Domain.Types.FarePolicy.RentalFarePolicy (RentalFarePolicy)
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import EulerHS.Prelude
import Utils.Common

type TripStartTime = UTCTime

type TripStopTime = UTCTime

data RentalFareParameters = RentalFareParameters
  { baseFare :: Amount,
    extraDistanceFare :: Amount,
    extraTimeFare :: Amount,
    nextDaysFare :: Maybe Amount, --use 0 instead of Nothing?
    discount :: Maybe Amount,
    farePolicy :: RentalFarePolicy
  }
  deriving stock (Show, Eq)

rentalFareSum :: RentalFareParameters -> Amount
rentalFareSum RentalFareParameters {..} = do
  let nextDaysFare' = fromMaybe 0 nextDaysFare
  baseFare + extraDistanceFare + extraTimeFare + nextDaysFare'

rentalFareSumWithDiscount :: RentalFareParameters -> Amount
rentalFareSumWithDiscount fp@RentalFareParameters {..} = do
  let fareSumm = rentalFareSum fp
  max 0 $ maybe fareSumm (fareSumm -) discount

calculateRentalFareParameters ::
  DRentalFP.RentalFarePolicy ->
  HighPrecMeters ->
  TripStartTime ->
  TripStopTime ->
  RentalFareParameters
calculateRentalFareParameters farePolicy distance startTime stopTime = do
  let baseFare = farePolicy.baseFare
      extraDistanceFare = calculateExtraDistanceFare farePolicy distance
      extraTimeFare = calculateExtraTimeFare farePolicy startTime stopTime
      nextDaysFare = calculateNextDaysFare farePolicy startTime stopTime
      discount = Nothing -- rental discount is not implemented yet
  RentalFareParameters {..}

calculateExtraDistanceFare ::
  RentalFarePolicy ->
  HighPrecMeters ->
  Amount
calculateExtraDistanceFare farePolicy distance = do
  let distanceInKm = metersToKilometers $ highPrecMetersToMeters distance
  let extraDistance = distanceInKm - farePolicy.baseDistance
  if extraDistance > 0
    then Amount (toRational extraDistance) * farePolicy.extraKmFare
    else 0

calculateExtraTimeFare ::
  RentalFarePolicy ->
  TripStartTime ->
  TripStopTime ->
  Amount
calculateExtraTimeFare farePolicy tripStartTime tripStopTime = do
  let tripTime = diffUTCTime tripStopTime tripStartTime
      tripTimeInMinutes = nominalDiffTimeToSeconds tripTime `div` 60
      extraTime = toInteger tripTimeInMinutes - toInteger farePolicy.baseDuration * 60
  if extraTime > 0
    then Amount (toRational extraTime) * farePolicy.extraMinuteFare
    else 0

calculateNextDaysFare ::
  RentalFarePolicy ->
  TripStartTime ->
  TripStopTime ->
  Maybe Amount
calculateNextDaysFare farePolicy tripStartTime tripStopTime = do
  let tripDays = calcTripDays tripStartTime tripStopTime
  if tripDays > 0
    then calcNextDaysFare tripDays <$> farePolicy.driverAllowanceForDay
    else Nothing
  where
    calcNextDaysFare :: Integer -> Amount -> Amount
    calcNextDaysFare tripDays nextDaysFare = Amount (toRational tripDays) * nextDaysFare

timeZoneIST :: TimeZone
timeZoneIST = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530

-- Calculate how many days the trip lasts.
-- Every day is added at midnight local time
calcTripDays :: TripStartTime -> TripStopTime -> Integer
calcTripDays tripStartTime tripStopTime = diffDays (calcDay tripStopTime) (calcDay tripStartTime)
  where
    calcDay = localDay . utcToLocalTime timeZoneIST
