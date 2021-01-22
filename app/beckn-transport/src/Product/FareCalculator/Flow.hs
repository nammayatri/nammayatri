{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module Product.FareCalculator.Flow where

import Beckn.Product.BusinessRule
import Beckn.Types.Amount
import Beckn.Types.ID
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time
import EulerHS.Prelude
import Product.FareCalculator.Models.FarePolicy

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
    getDistance :: PickupLocation -> DropLocation -> BusinessRule m Float
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

calculateFare ::
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
calculateFare ServiceHandle {..} orgId vehicleVariant pickupLoc dropLoc journeyType startTime mbDistance = do
  farePolicy <- getFarePolicy orgId vehicleVariant >>= fromMaybeBR "NO_FARE_POLICY"
  actualDistance <- mbDistance & maybe (getDistance pickupLoc dropLoc) pure
  baseFare <- calculateBaseFare farePolicy actualDistance
  distanceFare <- calculateDistanceFare farePolicy actualDistance journeyType
  nightShiftRate <- calculateNightShiftRate farePolicy startTime
  pure $ FareParameters baseFare distanceFare nightShiftRate

calculateBaseFare ::
  Monad m =>
  FarePolicy ->
  DistanceInM ->
  BusinessRule m Amount
calculateBaseFare farePolicy _actualDistance = do
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
  let journeyRate = case journeyType of
        OneWayTrip -> 1.0
        HalfReturnTrip -> 1.5
        FullReturnTrip -> 2.0
  let baseDistanceFare =
        if toRational actualDistance > baseDistance
          then ((toRational actualDistance * journeyRate) - baseDistance) / 1000
          else 0
  pure . Amount $ baseDistanceFare * perKmRate

calculateNightShiftRate ::
  Monad m =>
  FarePolicy ->
  TripStartTime ->
  BusinessRule m Amount
calculateNightShiftRate farePolicy startTime = do
  let timeOfDay = timeToTimeOfDay $ utctDayTime startTime
  let nightShiftRate = farePolicy ^. #nightShiftRate
  let nightShiftStart = farePolicy ^. #nightShiftStart
  let nightShiftEnd = farePolicy ^. #nightShiftEnd
  pure . Amount $
    if timeOfDay > nightShiftStart || timeOfDay < nightShiftEnd
      then nightShiftRate
      else 1
