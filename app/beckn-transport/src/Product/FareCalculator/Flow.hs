{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Product.FareCalculator.Flow where

import Beckn.Types.Amount (Amount (..))
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
  ( LocalTime (localTimeOfDay),
    UTCTime,
    midnight,
    minutesToTimeZone,
    utcToLocalTime,
  )
import EulerHS.Prelude
import Types.Domain.FarePolicy (FarePolicy, PerExtraKmRate (..))
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
  TripStartTime ->
  m FareParameters
doCalculateFare ServiceHandle {..} orgId vehicleVariant distanceSrc startTime = do
  farePolicy <- getFarePolicy orgId vehicleVariant >>= fromMaybeM NoFarePolicy
  actualDistance <-
    distanceSrc
      & fromEitherM' (uncurry getDistance >=> fromMaybeM CantCalculateDistance)
  baseFare <- calculateBaseFare farePolicy
  distanceFare <- calculateDistanceFare farePolicy actualDistance
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
  m Amount
calculateDistanceFare farePolicy actualDistance = do
  let sortedPerExtraKmRateList = NonEmpty.sortBy (compare `on` (.distanceRangeStart)) farePolicy.perExtraKmRateList -- sort it again just in case
  let baseDistance = (.distanceRangeStart) $ NonEmpty.head sortedPerExtraKmRateList
      extraDistance = toRational actualDistance - baseDistance
  if extraDistance <= 0
    then return $ Amount 0
    else do
      pure . Amount $ calculateExtraDistFare 0 extraDistance sortedPerExtraKmRateList
  where
    calculateExtraDistFare summ extraDist (PerExtraKmRate lowerBorder perKmRate :| sndPerExtraKmRate@(PerExtraKmRate upperBorder _) : perExtraKmRateList) = do
      let boundSize = upperBorder - lowerBorder
      let distWithinBounds = min extraDist boundSize
          fareWithinBounds = distWithinBounds / 1000 * perKmRate
      calculateExtraDistFare (summ + fareWithinBounds) (extraDist - distWithinBounds) (sndPerExtraKmRate :| perExtraKmRateList)
    calculateExtraDistFare summ 0 _ = summ
    calculateExtraDistFare summ extraDist (PerExtraKmRate _ perKmRate :| []) = summ + (extraDist / 1000 * perKmRate)

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
