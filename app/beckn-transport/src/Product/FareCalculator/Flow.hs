{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module Product.FareCalculator.Flow where

import Beckn.Types.Amount
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time
import EulerHS.Prelude
import Product.FareCalculator.BusinessRule
import Product.FareCalculator.Models.FareConfig
import Product.FareCalculator.Models.ID

newtype PickupLocation = PickupLocation {getPickupLocation :: Location.Location}
  deriving newtype (Show, Eq)

newtype DropLocation = DropLocation {getDropLocation :: Location.Location}
  deriving newtype (Show, Eq)

type TripStartTime = UTCTime

type DistanceInM = Float

data JourneyTrip = OneWayTrip | HalfReturnTrip | FullReturnTrip
  deriving stock (Show, Eq)

data ServiceHandle m = ServiceHandle
  { getFareConfig :: ID Organization.Organization -> Vehicle.Variant -> BusinessRule m (Maybe FareConfig),
    getDistance :: PickupLocation -> DropLocation -> BusinessRule m Float
  }

data FareParameters = FareParameters
  { baseFare :: Amount,
    distanceFare :: Amount,
    deadDistanceFare :: Amount,
    nightShiftRatio :: Amount
  }
  deriving stock (Show, Eq)

fareSum :: FareParameters -> Amount
fareSum FareParameters {..} = nightShiftRatio * (baseFare + distanceFare + deadDistanceFare)

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
  DistanceInM ->
  BusinessRule m FareParameters
calculateFare sh@ServiceHandle {..} orgId vehicleVariant pickupLoc dropLoc journeyType startTime mbDistance deadDistance = do
  fareConfig <- getFareConfig orgId vehicleVariant >>= fromMaybeBR "NO_FARE_CONFIG"
  actualDistance <- mbDistance & maybe (getDistance pickupLoc dropLoc) pure
  baseFare <- calculateBaseFare sh fareConfig actualDistance
  distanceFare <- calculateDistanceFare sh fareConfig actualDistance journeyType
  deadDistanceFare <- calculateDeadDistanceFare sh fareConfig deadDistance
  nightShiftRatio <- calculateNightShiftRatio sh fareConfig startTime
  pure $ FareParameters baseFare distanceFare deadDistanceFare nightShiftRatio

calculateBaseFare ::
  Monad m =>
  ServiceHandle m ->
  FareConfig ->
  DistanceInM ->
  BusinessRule m Amount
calculateBaseFare ServiceHandle {..} fareConfig _actualDistance = do
  let baseFare = fromMaybe 0 $ fareConfig ^. #baseFare
  pure $ Amount baseFare

calculateDistanceFare ::
  Monad m =>
  ServiceHandle m ->
  FareConfig ->
  DistanceInM ->
  JourneyTrip ->
  BusinessRule m Amount
calculateDistanceFare ServiceHandle {..} fareConfig actualDistance journeyType = do
  let baseDistance = fromMaybe 0 $ fareConfig ^. #baseDistance
  let perKmRate = fareConfig ^. #perExtraKmRate
  let journeyRate = case journeyType of
        OneWayTrip -> 1.0
        HalfReturnTrip -> 1.5
        FullReturnTrip -> 2.0
  let baseDistanceFare =
        if toRational actualDistance > baseDistance
          then ((toRational actualDistance * journeyRate) - baseDistance) / 1000
          else 0
  pure . Amount $ baseDistanceFare * perKmRate

calculateDeadDistanceFare ::
  Monad m =>
  ServiceHandle m ->
  FareConfig ->
  DistanceInM ->
  BusinessRule m Amount
calculateDeadDistanceFare ServiceHandle {..} fareConfig deadDistance = do
  let minDeadDistance = fromMaybe 0 $ fareConfig ^. #minDeadKmThreshold
  let perDeadKmRate = fareConfig ^. #perDeadKmRate
  let deadDistanceFare =
        if toRational deadDistance > minDeadDistance
          then (toRational deadDistance - minDeadDistance) / 1000
          else 0
  pure . Amount $ deadDistanceFare * perDeadKmRate

calculateNightShiftRatio ::
  Monad m =>
  ServiceHandle m ->
  FareConfig ->
  TripStartTime ->
  BusinessRule m Amount
calculateNightShiftRatio ServiceHandle {..} _fareConfig _startTime = pure 1
