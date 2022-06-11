{-# LANGUAGE DerivingStrategies #-}

module Product.FareCalculator.Calculator where

import Beckn.Types.Amount (Amount (..))
import Beckn.Types.Common
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
  ( LocalTime (localTimeOfDay),
    TimeOfDay (..),
    TimeZone,
    midnight,
    minutesToTimeZone,
    utcToLocalTime,
  )
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.FarePolicy.PerExtraKmRate (PerExtraKmRate (..))
import EulerHS.Prelude
import Utils.Common

type TripStartTime = UTCTime

data FareParameters = FareParameters
  { baseFare :: Amount,
    distanceFare :: Amount,
    nightShiftRate :: Amount
  }
  deriving stock (Show, Eq)

fareSum :: FareParameters -> Amount
fareSum FareParameters {..} =
  nightShiftRate * (baseFare + distanceFare)

calculateFareParameters ::
  FarePolicy ->
  Meter ->
  TripStartTime ->
  FareParameters
calculateFareParameters farePolicy distance startTime = do
  let baseFare = calculateBaseFare farePolicy
  let distanceFare = calculateDistanceFare farePolicy distance
  let nightShiftRate = calculateNightShiftRate farePolicy startTime
  FareParameters baseFare distanceFare nightShiftRate

calculateBaseFare ::
  FarePolicy ->
  Amount
calculateBaseFare farePolicy = do
  let baseFare = fromMaybe 0 $ farePolicy.baseFare
  Amount baseFare

calculateDistanceFare ::
  FarePolicy ->
  Meter ->
  Amount
calculateDistanceFare farePolicy distance = do
  let sortedPerExtraKmRateList = NonEmpty.sortBy (compare `on` (.distanceRangeStart)) farePolicy.perExtraKmRateList -- sort it again just in case
  let baseDistance = (.distanceRangeStart) $ NonEmpty.head sortedPerExtraKmRateList
      extraDistance = toRational (getDistanceInMeter distance) - baseDistance
  if extraDistance <= 0
    then 0
    else do
      Amount $ calculateExtraDistFare 0 extraDistance sortedPerExtraKmRateList
  where
    calculateExtraDistFare summ extraDist (PerExtraKmRate lowerBorder perKmRate :| sndPerExtraKmRate@(PerExtraKmRate upperBorder _) : perExtraKmRateList) = do
      let boundSize = upperBorder - lowerBorder
      let distWithinBounds = min extraDist boundSize
          fareWithinBounds = distWithinBounds / 1000 * perKmRate
      calculateExtraDistFare (summ + fareWithinBounds) (extraDist - distWithinBounds) (sndPerExtraKmRate :| perExtraKmRateList)
    calculateExtraDistFare summ 0 _ = summ
    calculateExtraDistFare summ extraDist (PerExtraKmRate _ perKmRate :| []) = summ + (extraDist / 1000 * perKmRate)

calculateNightShiftRate ::
  FarePolicy ->
  TripStartTime ->
  Amount
calculateNightShiftRate farePolicy tripStartTime = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZoneIST tripStartTime
  let nightShiftRate = fromMaybe 1 $ farePolicy.nightShiftRate
  let nightShiftStart = fromMaybe midnight $ farePolicy.nightShiftStart
  let nightShiftEnd = fromMaybe midnight $ farePolicy.nightShiftEnd
  Amount $
    if isTimeWithinBounds nightShiftStart nightShiftEnd timeOfDay
      then nightShiftRate
      else 1

timeZoneIST :: TimeZone
timeZoneIST = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530

isTimeWithinBounds :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
isTimeWithinBounds startTime endTime time =
  if startTime >= endTime
    then do
      let midnightBeforeTimeleap = TimeOfDay 23 59 60
      (startTime < time && time < midnightBeforeTimeleap) || (midnight <= time && time < endTime)
    else startTime < time && time < endTime
