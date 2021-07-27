{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Product.FareCalculator.Flow where

import Beckn.Types.Amount (Amount (..))
import Beckn.Types.Common
import Beckn.Types.Id (Id)
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
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.SearchReqLocation as Location
import qualified Types.Storage.Vehicle as Vehicle
import Utils.Common

newtype PickupLocation = PickupLocation {getPickupLocation :: Location.SearchReqLocation}
  deriving newtype (Show, Eq)

newtype DropLocation = DropLocation {getDropLocation :: Location.SearchReqLocation}
  deriving newtype (Show, Eq)

type TripStartTime = UTCTime

type DistanceInM = Double

data JourneyTrip = OneWayTrip | HalfReturnTrip | FullReturnTrip
  deriving stock (Show, Eq)

type MonadHandler m = (MonadThrow m, Log m)

data ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Organization.Organization -> Vehicle.Variant -> m (Maybe FarePolicy),
    getDistance :: PickupLocation -> DropLocation -> m (Maybe Double)
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
  Either
    ( PickupLocation,
      DropLocation
    )
    DistanceInM ->
  JourneyTrip ->
  TripStartTime ->
  m FareParameters
doCalculateFare ServiceHandle {..} orgId vehicleVariant distanceSrc journeyType startTime = do
  farePolicy <- getFarePolicy orgId vehicleVariant >>= fromMaybeM NoFarePolicy
  actualDistance <-
    distanceSrc
      & fromEitherM' (uncurry getDistance >=> fromMaybeM CantCalculateDistance)
  baseFare <- calculateBaseFare farePolicy
  distanceFare <- calculateDistanceFare farePolicy actualDistance journeyType
  nightShiftRate <- calculateNightShiftRate farePolicy startTime
  pure $ FareParameters baseFare distanceFare nightShiftRate

calculateBaseFare ::
  MonadHandler m =>
  FarePolicy ->
  m Amount
calculateBaseFare farePolicy = do
  let baseFare = fromMaybe 0 $ farePolicy.baseFare
  pure $ Amount baseFare

calculateDistanceFare ::
  MonadHandler m =>
  FarePolicy ->
  DistanceInM ->
  JourneyTrip ->
  m Amount
calculateDistanceFare farePolicy actualDistance journeyType = do
  let baseDistance = fromMaybe 0 $ farePolicy.baseDistance
  let perKmRate = farePolicy.perExtraKmRate
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
  let nightShiftRate = fromMaybe 1 $ farePolicy.nightShiftRate
  let nightShiftStart = fromMaybe midnight $ farePolicy.nightShiftStart
  let nightShiftEnd = fromMaybe midnight $ farePolicy.nightShiftEnd
  pure . Amount $
    if timeOfDay > nightShiftStart || timeOfDay < nightShiftEnd
      then nightShiftRate
      else 1
