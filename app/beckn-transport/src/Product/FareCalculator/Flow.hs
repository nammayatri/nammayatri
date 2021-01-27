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

newtype DropLocation = DropLocation {getDropLocation :: Location.Location}

type TripStartTime = UTCTime

type DistanceInM = Float

data ServiceHandle m = ServiceHandle
  { getFareConfig :: ID Organization.Organization -> Vehicle.Variant -> BusinessRule m (Maybe FareConfig),
    getDistance :: PickupLocation -> DropLocation -> BusinessRule m Float
  }

calculateFare ::
  Monad m =>
  ServiceHandle m ->
  ID Organization.Organization ->
  Vehicle.Variant ->
  PickupLocation ->
  DropLocation ->
  TripStartTime ->
  Maybe DistanceInM ->
  BusinessRule m Amount
calculateFare sh@ServiceHandle {..} orgId vehicleVariant pickupLoc dropLoc startTime mbDistance = do
  fareConfig <-
    getFareConfig orgId vehicleVariant
      >>= maybe
        (throwBusinessError "NO_FARE_CONFIG" $ "No FareConfig found for " +|| orgId ||+ " with vehicle type " +|| vehicleVariant ||+ "")
        pure
  actualDistance <- mbDistance & maybe (getDistance pickupLoc dropLoc) pure
  distanceFare <- calculateDistanceFare sh fareConfig actualDistance
  baseFare <- calculateBaseFare sh fareConfig actualDistance
  additionalFare <- calculateAdditionalFare sh fareConfig startTime
  pure $ distanceFare + baseFare + additionalFare

calculateDistanceFare :: Monad m => ServiceHandle m -> FareConfig -> DistanceInM -> BusinessRule m Amount
calculateDistanceFare ServiceHandle {..} fareConfig actualDistance =
  case fareConfig ^. #minimumDistance of
    Nothing -> pure 0
    Just minimumDistance -> do
      let perKmRate = fareConfig ^. #perKmRate
      let distance = toRational $ if actualDistance <= minimumDistance then minimumDistance else actualDistance
      pure $ Amount $ perKmRate * (distance / 1000)

calculateBaseFare :: Monad m => ServiceHandle m -> FareConfig -> DistanceInM -> BusinessRule m Amount
calculateBaseFare ServiceHandle {..} _fareConfig _actualDistance = pure 0

calculateAdditionalFare :: Monad m => ServiceHandle m -> FareConfig -> TripStartTime -> BusinessRule m Amount
calculateAdditionalFare ServiceHandle {..} _fareConfig _startTime = pure 0
