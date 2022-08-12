module Product.FareCalculator.Calculator
  ( mkBreakupList,
    fareSumRounded,
    baseFareSumRounded,
    calculateFareParameters,
    driverSelectedFareRounded,
  )
where

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
import Types.Money (RoundedMoney)
import Utils.Common

type TripStartTime = UTCTime

type Distance = HighPrecMeters

mkBreakupList :: (RoundedMoney -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkBreakupList mkPrice mkBreakupItem fareParams = do
  -- TODO: what should be here?
  let dayPartRate = calculateDayPartRate fareParams
      fareForPickupFinalRounded = roundToIntegral $ fareParams.baseFare * dayPartRate
      fareForPickupCaption = mconcat ["Base fare: ", show fareForPickupFinalRounded, " INR"]
      fareForPickupItem = mkBreakupItem fareForPickupCaption (mkPrice fareForPickupFinalRounded)

      mbExtraKmFareRounded = fareParams.extraKmFare <&> roundToIntegral . (* dayPartRate)
      extraDistanceFareCaption extraKmFare = mconcat ["Extra distance fare: ", show extraKmFare, " INR"]
      extraDistanceFareItem =
        mbExtraKmFareRounded <&> \extraKmFareRounded ->
          mkBreakupItem (extraDistanceFareCaption extraKmFareRounded) (mkPrice extraKmFareRounded)

      mkSelectedFareCaption selFare = mconcat ["Fare selected by driver: ", show selFare, " INR"]
      mbSelectedFareItem =
        fareParams.driverSelectedFare <&> \selFare ->
          mkBreakupItem (mkSelectedFareCaption $ roundToIntegral @_ @Int selFare) (mkPrice $ roundToIntegral selFare)

      totalFareFinalRounded = fareSumRounded fareParams
      totalFareCaption = mconcat ["Total fare: ", show totalFareFinalRounded, " INR"]
      totalFareItem = mkBreakupItem totalFareCaption $ mkPrice totalFareFinalRounded
  catMaybes [Just totalFareItem, Just fareForPickupItem, extraDistanceFareItem, mbSelectedFareItem]

-- TODO: make some tests for it
fareSum :: FareParameters -> Amount
fareSum fareParams = do
  baseFareSum fareParams + fromMaybe 0 fareParams.driverSelectedFare

fareSumRounded :: FareParameters -> RoundedMoney
fareSumRounded = roundToIntegral . fareSum

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

baseFareSumRounded :: FareParameters -> RoundedMoney
baseFareSumRounded = roundToIntegral . baseFareSum

getDriverSelectedFare :: FareParameters -> Amount
getDriverSelectedFare fp = fromMaybe 0 fp.driverSelectedFare

driverSelectedFareRounded :: FareParameters -> Amount
driverSelectedFareRounded = roundToUnits . getDriverSelectedFare

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
