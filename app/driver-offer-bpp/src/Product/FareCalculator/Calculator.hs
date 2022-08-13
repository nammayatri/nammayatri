{-# LANGUAGE DerivingStrategies #-}

module Product.FareCalculator.Calculator where

import Beckn.Prelude
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
import Domain.Types.FarePolicy
import Utils.Common

type TripStartTime = UTCTime

type Distance = HighPrecMeters

mkBreakupList :: (Amount -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkBreakupList mkPrice mkBreakupItem fareParams = do
  -- TODO: what should be here?
  let dayPartRate = calculateDayPartRate fareParams
      fareForPickupFinal = fareParams.baseFare * dayPartRate
      fareForPickupCaption = mconcat ["Base fare: ", amountToString fareForPickupFinal, " INR"]
      fareForPickupItem = mkBreakupItem fareForPickupCaption (mkPrice fareForPickupFinal)

      mbExtraKmFare = fareParams.extraKmFare <&> (* dayPartRate)
      extraDistanceFareCaption extraKmFare = mconcat ["Extra distance fare: ", amountToString extraKmFare, " INR"]
      extraDistanceFareItem =
        mbExtraKmFare <&> \extraKmFare ->
          mkBreakupItem (extraDistanceFareCaption extraKmFare) (mkPrice extraKmFare)

      mkSelectedFareCaption selFare = mconcat ["Fare selected by driver: ", amountToString selFare, " INR"]
      mbSelectedFareItem =
        fareParams.driverSelectedFare <&> \selFare ->
          mkBreakupItem (mkSelectedFareCaption selFare) (mkPrice selFare)

      totalFareFinal = fareSum fareParams
      totalFareCaption = mconcat ["Total fare: ", amountToString totalFareFinal, " INR"]
      totalFareItem = mkBreakupItem totalFareCaption $ mkPrice totalFareFinal
  catMaybes [Just totalFareItem, Just fareForPickupItem, extraDistanceFareItem, mbSelectedFareItem]

-- TODO: make some tests for it
fareSum :: FareParameters -> Amount
fareSum fareParams = do
  baseFareSum fareParams + fromMaybe 0 fareParams.driverSelectedFare

baseFareSum :: FareParameters -> Amount
baseFareSum fareParams = do
  let dayPartCoef = calculateDayPartRate fareParams
  dayPartCoef
    * sum
      ( catMaybes
          [ Just fareParams.baseFare,
            fareParams.extraKmFare
          ]
      )

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
calculateFareParameters fp distance startTime mbExtraFare = do
  let baseDistanceFare = fp.baseDistancePerKmFare
      mbExtraDistance =
        distance - fp.baseDistance
          & (\dist -> if dist > 0 then Just dist else Nothing)
      mbExtraKmFare = mbExtraDistance <&> \ex -> distanceToAmountKm ex * fp.extraKmFare
      nightCoefIncluded = defineWhetherNightCoefIncluded fp startTime

  FareParameters
    { baseFare = fp.deadKmFare + baseDistanceFare,
      extraKmFare = mbExtraKmFare,
      driverSelectedFare = mbExtraFare,
      nightShiftRate = fp.nightShiftRate,
      nightCoefIncluded
    }

distanceToAmountKm :: HighPrecMeters -> Amount
distanceToAmountKm x = realToFrac $ x.getHighPrecMeters / 1000

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
