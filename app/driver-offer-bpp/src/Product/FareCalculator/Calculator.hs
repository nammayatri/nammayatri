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
import Domain.Types.FarePolicy (FarePolicy)
import EulerHS.Prelude
import Utils.Common

type TripStartTime = UTCTime

type Distance = HighPrecMeters

data FareParameters = FareParameters
  { fareForPickup :: Amount,
    distanceFare :: Amount,
    nightShiftRate :: Amount
  }
  deriving stock (Show, Eq)

fareSum :: FareParameters -> Amount
fareSum FareParameters {..} =
  nightShiftRate * (fareForPickup + distanceFare)

calculateFareParameters ::
  FarePolicy ->
  Distance ->
  TripStartTime ->
  FareParameters
calculateFareParameters farePolicy distance startTime = do
  let fareForPickup = calculateBaseFare farePolicy
  let distanceFare = calculateDistanceFare farePolicy distance
  let nightShiftRate = calculateNightShiftRate farePolicy startTime
  FareParameters fareForPickup distanceFare nightShiftRate

calculateBaseFare ::
  FarePolicy ->
  Amount
calculateBaseFare farePolicy = farePolicy.fareForPickup

calculateDistanceFare ::
  FarePolicy ->
  Distance ->
  Amount
calculateDistanceFare farePolicy distance = farePolicy.farePerKm * (Amount $ toRational distance.getHighPrecMeters)

calculateNightShiftRate ::
  FarePolicy ->
  TripStartTime ->
  Amount
calculateNightShiftRate farePolicy tripStartTime = do
  let defaultNightShiftRate = 1
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZoneIST tripStartTime
  let nightShiftRate = fromMaybe defaultNightShiftRate $ farePolicy.nightShiftRate
  let nightShiftStart = fromMaybe midnight $ farePolicy.nightShiftStart
  let nightShiftEnd = fromMaybe midnight $ farePolicy.nightShiftEnd
  if isTimeWithinBounds nightShiftStart nightShiftEnd timeOfDay
    then nightShiftRate
    else defaultNightShiftRate

timeZoneIST :: TimeZone
timeZoneIST = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530

isTimeWithinBounds :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
isTimeWithinBounds startTime endTime time =
  if startTime >= endTime
    then do
      let midnightBeforeTimeleap = TimeOfDay 23 59 60
      (startTime < time && time < midnightBeforeTimeleap) || (midnight <= time && time < endTime)
    else startTime < time && time < endTime
