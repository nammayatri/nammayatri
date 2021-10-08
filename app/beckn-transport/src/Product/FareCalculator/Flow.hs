{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Product.FareCalculator.Flow where

import Beckn.Types.Amount (Amount (..))
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
  ( LocalTime (localTimeOfDay),
    TimeOfDay (..),
    TimeZone,
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

newtype ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Organization.Organization -> Vehicle.Variant -> m (Maybe FarePolicy)
  }

data FareParameters = FareParameters
  { baseFare :: Amount,
    distanceFare :: Amount,
    nightShiftRate :: Amount,
    discount :: Maybe Amount
  }
  deriving stock (Show, Eq)

doCalculateFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Organization.Organization ->
  Vehicle.Variant ->
  DistanceInM ->
  TripStartTime ->
  m FareParameters
doCalculateFare ServiceHandle {..} orgId vehicleVariant distance startTime = do
  farePolicy <- getFarePolicy orgId vehicleVariant >>= fromMaybeM NoFarePolicy
  baseFare <- calculateBaseFare farePolicy
  distanceFare <- calculateDistanceFare farePolicy distance
  nightShiftRate <- calculateNightShiftRate farePolicy startTime
  let discount = calculateDiscount farePolicy startTime
  pure $ FareParameters baseFare distanceFare nightShiftRate discount

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
calculateDistanceFare farePolicy distance = do
  let sortedPerExtraKmRateList = NonEmpty.sortBy (compare `on` (.distanceRangeStart)) farePolicy.perExtraKmRateList -- sort it again just in case
  let baseDistance = (.distanceRangeStart) $ NonEmpty.head sortedPerExtraKmRateList
      extraDistance = toRational distance - baseDistance
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
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZone startTime
  let nightShiftRate = fromMaybe 1 $ farePolicy.nightShiftRate
  let nightShiftStart = fromMaybe midnight $ farePolicy.nightShiftStart
  let nightShiftEnd = fromMaybe midnight $ farePolicy.nightShiftEnd
  pure . Amount $
    if isTimeWithinBounds nightShiftStart nightShiftEnd timeOfDay
      then nightShiftRate
      else 1

calculateDiscount :: FarePolicy -> TripStartTime -> Maybe Amount
calculateDiscount farePolicy startTime = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZone startTime
      discount = calculateDiscount' 0 timeOfDay farePolicy.discountList
  if discount <= 0 then Nothing else Just $ Amount discount
  where
    calculateDiscount' summ timeOfDay (discount : discountList) = do
      if discount.enabled && isTimeWithinBounds discount.startTime discount.endTime timeOfDay
        then calculateDiscount' (summ + discount.discount) timeOfDay discountList
        else calculateDiscount' summ timeOfDay discountList
    calculateDiscount' summ _ [] = summ

timeZone :: TimeZone
timeZone = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530

isTimeWithinBounds :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
isTimeWithinBounds startTime endTime time =
  if startTime >= endTime
    then do
      let midnightBeforeTimeleap = TimeOfDay 23 59 60
      (startTime < time && time < midnightBeforeTimeleap) || (midnight <= time && time < endTime)
    else startTime < time && time < endTime