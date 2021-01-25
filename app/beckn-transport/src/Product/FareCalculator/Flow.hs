{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module Product.FareCalculator.Flow where

import Beckn.Product.BusinessRule (BusinessRule, fromMaybeBR)
import Beckn.Types.Amount (Amount (..))
import Beckn.Types.ID (ID)
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time
  ( LocalTime (localTimeOfDay),
    UTCTime,
    minutesToTimeZone,
    utcToLocalTime,
  )
import EulerHS.Prelude
import Product.FareCalculator.Models.FarePolicy (FarePolicy)

newtype PickupLocation = PickupLocation {getPickupLocation :: Location.Location}
  deriving newtype (Show, Eq)

newtype DropLocation = DropLocation {getDropLocation :: Location.Location}
  deriving newtype (Show, Eq)

type TripStartTime = UTCTime

type DistanceInM = Float

data JourneyTrip = OneWayTrip | HalfReturnTrip | FullReturnTrip
  deriving stock (Show, Eq)

data ServiceHandle m = ServiceHandle
  { getFarePolicy :: ID Organization.Organization -> Vehicle.Variant -> BusinessRule m (Maybe FarePolicy),
    getDistance :: PickupLocation -> DropLocation -> BusinessRule m (Maybe Float)
  }

data FareParameters = FareParameters
  { baseFare :: Amount,
    distanceFare :: Amount,
    nightShiftRate :: Amount
  }
  deriving stock (Show, Eq)

fareSum :: FareParameters -> Amount
fareSum FareParameters {..} =
  nightShiftRate * (baseFare + distanceFare)

doCalculateFare ::
  Monad m =>
  ServiceHandle m ->
  ID Organization.Organization ->
  Vehicle.Variant ->
  PickupLocation ->
  DropLocation ->
  JourneyTrip ->
  TripStartTime ->
  Maybe DistanceInM ->
  BusinessRule m FareParameters
doCalculateFare ServiceHandle {..} orgId vehicleVariant pickupLoc dropLoc journeyType startTime mbDistance = do
  farePolicy <- getFarePolicy orgId vehicleVariant >>= fromMaybeBR "NO_FARE_POLICY"
  actualDistance <-
    mbDistance
      & maybe
        ( getDistance pickupLoc dropLoc
            >>= fromMaybeBR "CANT_CALCULATE_DISTANCE"
        )
        pure
  baseFare <- calculateBaseFare farePolicy
  distanceFare <- calculateDistanceFare farePolicy actualDistance journeyType
  nightShiftRate <- calculateNightShiftRate farePolicy startTime
  pure $ FareParameters baseFare distanceFare nightShiftRate

calculateBaseFare ::
  Monad m =>
  FarePolicy ->
  BusinessRule m Amount
calculateBaseFare farePolicy = do
  let baseFare = fromMaybe 0 $ farePolicy ^. #baseFare
  pure $ Amount baseFare

calculateDistanceFare ::
  Monad m =>
  FarePolicy ->
  DistanceInM ->
  JourneyTrip ->
  BusinessRule m Amount
calculateDistanceFare farePolicy actualDistance journeyType = do
  let baseDistance = fromMaybe 0 $ farePolicy ^. #baseDistance
  let perKmRate = farePolicy ^. #perExtraKmRate
  let distanceMultiplier = case journeyType of
        OneWayTrip -> 1.0
        HalfReturnTrip -> 1.5
        FullReturnTrip -> 2.0
  let baseDistanceFare =
        if toRational actualDistance > baseDistance
          then ((toRational actualDistance * distanceMultiplier) - baseDistance) / 1000
          else 0
  pure . Amount $ baseDistanceFare * perKmRate

calculateNightShiftRate ::
  Monad m =>
  FarePolicy ->
  TripStartTime ->
  BusinessRule m Amount
calculateNightShiftRate farePolicy startTime = do
  let timeZone = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZone startTime
  let nightShiftRate = farePolicy ^. #nightShiftRate
  let nightShiftStart = farePolicy ^. #nightShiftStart
  let nightShiftEnd = farePolicy ^. #nightShiftEnd
  pure . Amount $
    if timeOfDay > nightShiftStart || timeOfDay < nightShiftEnd
      then nightShiftRate
      else 1
