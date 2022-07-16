{-# LANGUAGE DerivingStrategies #-}

module Product.FareCalculator.Calculator where

import Beckn.Types.Amount
import Data.Time
  ( LocalTime (localTimeOfDay),
    TimeOfDay (..),
    TimeZone,
    midnight,
    minutesToTimeZone,
    utcToLocalTime,
  )
import Domain.Types.FareParams
import Domain.Types.FarePolicy (FarePolicy)
import EulerHS.Prelude
import Utils.Common

type TripStartTime = UTCTime

type Distance = HighPrecMeters

mkBreakupList :: (Amount -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkBreakupList mkPrice mkBreakupItem fareParams = do
  {-
    let dayPartRate = calculateDayPartRate fareParams
        fareForPickupFinal = fareParams.fareForPickup * dayPartRate
        fareForPickupCaption = mconcat ["Fare for pickup: ", show fareForPickupFinal, " INR"]
        fareForPickupItem = mkBreakupItem fareForPickupCaption (mkPrice fareForPickupFinal)

        distanceFareFinal = fareParams.distanceFare * dayPartRate
        distanceFareCaption = mconcat ["Distance fare: ", show distanceFareFinal, " INR"]
        distanceFareItem = mkBreakupItem distanceFareCaption (mkPrice distanceFareFinal)

        mkSelectedFareCaption selFare = mconcat ["Fare selected by driver: ", show selFare, " INR"]
        mbSelectedFareItem =
          fareParams.driverSelectedFare <&> \selFare ->
            mkBreakupItem (mkSelectedFareCaption selFare) (mkPrice selFare)
  -}
  let totalFareFinal = fareSum fareParams
      totalFareCaption = mconcat ["Total fare: ", show totalFareFinal, " INR"]
      totalFareItem = mkBreakupItem totalFareCaption $ mkPrice totalFareFinal
  --  catMaybes [Just totalFareItem, Just fareForPickupItem, Just distanceFareItem, mbSelectedFareItem]
  --  FIXME: improve spec?
  catMaybes [Just totalFareItem]

-- TODO: make some tests for it
fareSum :: FareParameters -> Amount
fareSum fareParams = do
  baseFareSum fareParams + fromMaybe 0 fareParams.driverSelectedFare

baseFareSum :: FareParameters -> Amount
baseFareSum fareParams = do
  let dayPartCoef = calculateDayPartRate fareParams
  dayPartCoef * (fareParams.fareForPickup + fareParams.distanceFare)

calculateDayPartRate :: FareParameters -> Amount
calculateDayPartRate fareParams = do
  let defaultDayPartRate = 1
  if fareParams.nightCoefIncluded
    then fromMaybe defaultDayPartRate fareParams.nightShiftRate
    else defaultDayPartRate

calculateFareParameters ::
  FarePolicy ->
  Distance ->
  TripStartTime ->
  Maybe Amount ->
  FareParameters
calculateFareParameters farePolicy distance startTime mbExtraFare = do
  let distanceFare = farePolicy.farePerKm * Amount (toRational $ distance.getHighPrecMeters / 1000)
  let nightCoefIncluded = defineWhetherNightCoefIncluded farePolicy startTime
  FareParameters farePolicy.fareForPickup distanceFare mbExtraFare farePolicy.nightShiftRate nightCoefIncluded

defineWhetherNightCoefIncluded ::
  FarePolicy ->
  TripStartTime ->
  Bool
defineWhetherNightCoefIncluded farePolicy tripStartTime = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZoneIST tripStartTime
  let nightShiftStart = fromMaybe midnight $ farePolicy.nightShiftStart
  let nightShiftEnd = fromMaybe midnight $ farePolicy.nightShiftEnd
  isTimeWithinBounds nightShiftStart nightShiftEnd timeOfDay

timeZoneIST :: TimeZone
timeZoneIST = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530

isTimeWithinBounds :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
isTimeWithinBounds startTime endTime time =
  if startTime >= endTime
    then do
      let midnightBeforeTimeleap = TimeOfDay 23 59 60
      (startTime < time && time < midnightBeforeTimeleap) || (midnight <= time && time < endTime)
    else startTime < time && time < endTime
