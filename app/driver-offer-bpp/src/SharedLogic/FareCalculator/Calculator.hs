module SharedLogic.FareCalculator.Calculator
  ( mkBreakupList,
    fareSumRounded,
    baseFareSumRounded,
    calculateFareParameters,
  )
where

import Beckn.Prelude
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

mkBreakupList :: (Money -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkBreakupList mkPrice mkBreakupItem fareParams = do
  -- TODO: what should be here?
  let dayPartRate = calculateDayPartRate fareParams
      fareForPickupFinalRounded = roundToIntegral $ fromIntegral fareParams.baseFare * dayPartRate
      fareForPickupCaption = mconcat ["Base fare: ", show fareForPickupFinalRounded, " INR"]
      fareForPickupItem = mkBreakupItem fareForPickupCaption (mkPrice fareForPickupFinalRounded)

      mbExtraKmFareRounded = fareParams.extraKmFare <&> roundToIntegral . (* dayPartRate) . fromIntegral
      extraDistanceFareCaption extraKmFare = mconcat ["Extra distance fare: ", show extraKmFare, " INR"]
      extraDistanceFareItem =
        mbExtraKmFareRounded <&> \extraKmFareRounded ->
          mkBreakupItem (extraDistanceFareCaption extraKmFareRounded) (mkPrice extraKmFareRounded)

      mkSelectedFareCaption selFare = mconcat ["Fare selected by driver: ", show selFare, " INR"]
      mbSelectedFareItem =
        fareParams.driverSelectedFare <&> \selFare ->
          mkBreakupItem (mkSelectedFareCaption selFare) (mkPrice selFare)

      totalFareFinalRounded = fareSumRounded fareParams
      totalFareCaption = mconcat ["Total fare: ", show totalFareFinalRounded, " INR"]
      totalFareItem = mkBreakupItem totalFareCaption $ mkPrice totalFareFinalRounded
  catMaybes [Just totalFareItem, Just fareForPickupItem, extraDistanceFareItem, mbSelectedFareItem]

-- TODO: make some tests for it
fareSumRounded :: FareParameters -> Money
fareSumRounded fareParams = do
  baseFareSumRounded fareParams + fromMaybe 0 fareParams.driverSelectedFare

baseFareSumRounded :: FareParameters -> Money
baseFareSumRounded fareParams = roundToIntegral $ do
  let dayPartCoef = calculateDayPartRate fareParams
  dayPartCoef
    * sum
      ( catMaybes
          [ Just $ fromIntegral fareParams.baseFare,
            fmap fromIntegral fareParams.extraKmFare
          ]
      )

calculateDayPartRate :: FareParameters -> Double
calculateDayPartRate fareParams = do
  let defaultDayPartRate = 1
  if fareParams.nightCoefIncluded
    then fromMaybe defaultDayPartRate fareParams.nightShiftRate
    else defaultDayPartRate

calculateFareParameters ::
  FarePolicy ->
  Meters ->
  TripStartTime ->
  Maybe Money ->
  FareParameters
calculateFareParameters fp distance startTime mbExtraFare = do
  let baseDistanceFare = roundToIntegral $ fp.baseDistancePerKmFare * realToFrac (distanceToKm fp.baseDistanceMeters)
      mbExtraDistance =
        distance - fp.baseDistanceMeters
          & (\dist -> if dist > 0 then Just dist else Nothing)
      mbExtraKmFare = mbExtraDistance <&> \ex -> roundToIntegral $ realToFrac (distanceToKm ex) * fp.perExtraKmFare
      nightCoefIncluded = defineWhetherNightCoefIncluded fp startTime

  FareParameters
    { baseFare = fp.deadKmFare + baseDistanceFare,
      extraKmFare = mbExtraKmFare,
      driverSelectedFare = mbExtraFare,
      nightShiftRate = fp.nightShiftRate,
      nightCoefIncluded
    }

distanceToKm :: Meters -> Rational
distanceToKm x = realToFrac x / 1000

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
