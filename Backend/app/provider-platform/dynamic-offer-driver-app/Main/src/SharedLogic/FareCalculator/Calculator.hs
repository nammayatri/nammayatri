{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FareCalculator.Calculator
  ( mkBreakupList,
    fareSum,
    baseFareSum,
    calculateFareParameters,
    isNightShift,
  )
where

import Data.Time
  ( LocalTime (localTimeOfDay),
    TimeOfDay (..),
    TimeZone,
    midnight,
    minutesToTimeZone,
    utcToLocalTime,
  )
import Domain.Types.FareParameters
import Domain.Types.FarePolicy
import Kernel.Prelude
import Kernel.Utils.Common

mkBreakupList :: (Money -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkBreakupList mkPrice mkBreakupItem fareParams = do
  -- TODO: what should be here?
  let dayPartRate = calculateDayPartRate fareParams
      baseFareFinalRounded = roundToIntegral $ fromIntegral fareParams.baseFare * dayPartRate + (maybe 0.0 fromIntegral fareParams.deadKmFare) -- TODO: Remove later part once UI start consuming DEAD_KILOMETER_FARE
      baseFareCaption = "BASE_FARE"
      baseFareItem = mkBreakupItem baseFareCaption (mkPrice baseFareFinalRounded)

      deadKmFareCaption = "DEAD_KILOMETER_FARE"
      deadKmFareItem =
        fareParams.deadKmFare <&> \deadKmFare ->
          mkBreakupItem deadKmFareCaption (mkPrice deadKmFare)

      mbExtraKmFareRounded = fareParams.extraKmFare <&> roundToIntegral . (* dayPartRate) . fromIntegral
      extraDistanceFareCaption = "EXTRA_DISTANCE_FARE"
      extraDistanceFareItem =
        mbExtraKmFareRounded <&> \extraKmFareRounded ->
          mkBreakupItem extraDistanceFareCaption (mkPrice extraKmFareRounded)

      mkSelectedFareCaption = "DRIVER_SELECTED_FARE"
      mbSelectedFareItem =
        fareParams.driverSelectedFare <&> \selFare ->
          mkBreakupItem mkSelectedFareCaption (mkPrice selFare)

      totalFareFinalRounded = fareSum fareParams
      totalFareCaption = "TOTAL_FARE"
      totalFareItem = mkBreakupItem totalFareCaption $ mkPrice totalFareFinalRounded
  catMaybes [Just totalFareItem, Just baseFareItem, deadKmFareItem, extraDistanceFareItem, mbSelectedFareItem]

-- TODO: make some tests for it
fareSum :: FareParameters -> Money
fareSum fareParams = do
  baseFareSum fareParams + (fromMaybe 0 fareParams.deadKmFare) + fromMaybe 0 fareParams.driverSelectedFare

baseFareSum :: FareParameters -> Money
baseFareSum fareParams = roundToIntegral $ do
  let dayPartCoef = calculateDayPartRate fareParams
  dayPartCoef
    * sum
      ( catMaybes
          [ Just $ fromIntegral fareParams.baseFare,
            fmap fromIntegral fareParams.extraKmFare
          ]
      )

calculateDayPartRate :: FareParameters -> Centesimal
calculateDayPartRate fareParams = do
  let defaultDayPartRate = 1
  if fareParams.nightCoefIncluded
    then fromMaybe defaultDayPartRate fareParams.nightShiftRate
    else defaultDayPartRate

calculateFareParameters ::
  MonadGuid m =>
  FarePolicy ->
  Meters ->
  UTCTime ->
  Maybe Money ->
  m FareParameters
calculateFareParameters fp distance time mbExtraFare = do
  let baseDistanceFare = roundToIntegral $ fp.baseDistanceFare
      mbExtraDistance =
        distance - fp.baseDistanceMeters
          & (\dist -> if dist > 0 then Just dist else Nothing)
      mbExtraKmFare = mbExtraDistance <&> \ex -> roundToIntegral $ realToFrac (distanceToKm ex) * fp.perExtraKmFare
      nightCoefIncluded = isNightShift fp time

  id <- generateGUID
  pure
    FareParameters
      { id,
        baseFare = baseDistanceFare,
        deadKmFare = Just fp.deadKmFare,
        extraKmFare = mbExtraKmFare,
        driverSelectedFare = mbExtraFare,
        nightShiftRate = fp.nightShiftRate,
        nightCoefIncluded,
        waitingChargePerMin = fp.waitingChargePerMin
      }

distanceToKm :: Meters -> Rational
distanceToKm x = realToFrac x / 1000

isNightShift ::
  FarePolicy ->
  UTCTime ->
  Bool
isNightShift farePolicy time = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZoneIST time
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
