{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module Product.FareCalculator.Flow where

import Beckn.Types.Amount (Amount (..))
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time
  ( LocalTime (localTimeOfDay),
    UTCTime,
    midnight,
    minutesToTimeZone,
    utcToLocalTime,
  )
import EulerHS.Prelude
import Types.Domain.FarePolicy (FarePolicy)
import Types.Error
import Utils.Common

newtype PickupLocation = PickupLocation {getPickupLocation :: Location.Location}
  deriving newtype (Show, Eq)

newtype DropLocation = DropLocation {getDropLocation :: Location.Location}
  deriving newtype (Show, Eq)

type TripStartTime = UTCTime

type DistanceInM = Float

data JourneyTrip = OneWayTrip | HalfReturnTrip | FullReturnTrip
  deriving stock (Show, Eq)

type MonadHandler m = (MonadThrow m, Log m)

data ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Organization.Organization -> Vehicle.Variant -> m (Maybe FarePolicy),
    getDistance :: PickupLocation -> DropLocation -> m (Maybe Float)
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
  MonadHandler m =>
  ServiceHandle m ->
  Id Organization.Organization ->
  Vehicle.Variant ->
  PickupLocation ->
  DropLocation ->
  JourneyTrip ->
  TripStartTime ->
  Maybe DistanceInM ->
  m FareParameters
doCalculateFare ServiceHandle {..} orgId vehicleVariant pickupLoc dropLoc journeyType startTime mbDistance = do
  farePolicy <- getFarePolicy orgId vehicleVariant >>= fromMaybeM NoFarePolicy
  actualDistance <-
    mbDistance
      & maybe
        ( getDistance pickupLoc dropLoc
            >>= fromMaybeM CantCalculateDistance
        )
        pure
  baseFare <- calculateBaseFare farePolicy
  distanceFare <- calculateDistanceFare farePolicy actualDistance journeyType
  nightShiftRate <- calculateNightShiftRate farePolicy startTime
  pure $ FareParameters baseFare distanceFare nightShiftRate

calculateBaseFare ::
  MonadHandler m =>
  FarePolicy ->
  m Amount
calculateBaseFare farePolicy = do
  let baseFare = fromMaybe 0 $ farePolicy ^. #baseFare
  pure $ Amount baseFare

calculateDistanceFare ::
  MonadHandler m =>
  FarePolicy ->
  DistanceInM ->
  JourneyTrip ->
  m Amount
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
  MonadHandler m =>
  FarePolicy ->
  TripStartTime ->
  m Amount
calculateNightShiftRate farePolicy startTime = do
  let timeZone = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZone startTime
  let nightShiftRate = fromMaybe 1 $ farePolicy ^. #nightShiftRate
  let nightShiftStart = fromMaybe midnight $ farePolicy ^. #nightShiftStart
  let nightShiftEnd = fromMaybe midnight $ farePolicy ^. #nightShiftEnd
  pure . Amount $
    if timeOfDay > nightShiftStart || timeOfDay < nightShiftEnd
      then nightShiftRate
      else 1
