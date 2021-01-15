{-# LANGUAGE OverloadedLabels #-}

module Product.FareCalculator.Flow where

import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time
import EulerHS.Prelude hiding (drop)
import Product.FareCalculator.BusinessRule
import Product.FareCalculator.Models.FareConfig
import Product.FareCalculator.Models.ID

newtype PickupLocation = PickupLocation {getPickupLocation :: Location.Location}

newtype DropLocation = DropLocation {getDropLocation :: Location.Location}

type TripStartTime = UTCTime

type DistanceInM = Float

data ServiceHandle m = ServiceHandle
  { getFareConfig :: ID Organization.Organization -> Vehicle.Variant -> BusinessRule m (Maybe FareConfig),
    getDistance :: Location.Location -> Location.Location -> BusinessRule m Float
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
  BusinessRule m Float
calculateFare sh@ServiceHandle {..} orgId vehicleVariant pickupLoc dropLoc startTime mbDistance = do
  fareConfig <-
    getFareConfig orgId vehicleVariant
      >>= maybe
        (throwBusinessError "NO_FARE_CONFIG" $ "No FareConfig found for " +|| orgId ||+ " with vehicle type " +|| vehicleVariant ||+ "")
        pure
  let pickup = getPickupLocation pickupLoc
  let drop = getDropLocation dropLoc
  actualDistance <- mbDistance & maybe (getDistance pickup drop) pure
  distanceFare <- computeDistanceFare sh fareConfig actualDistance
  baseFare <- computeBaseFare sh fareConfig actualDistance
  additionalFare <- computeAdditionalFare sh fareConfig startTime
  pure $ distanceFare + baseFare + additionalFare

computeDistanceFare :: Monad m => ServiceHandle m -> FareConfig -> DistanceInM -> BusinessRule m Float
computeDistanceFare ServiceHandle {..} fareConfig actualDistance = do
  case fareConfig ^. #minimumDistance of
    Nothing -> pure 0
    Just minimumDistance -> do
      let perKmRate = fareConfig ^. #perKmRate
      let distance = if actualDistance <= minimumDistance then minimumDistance else actualDistance
      pure $ perKmRate * (distance / 1000)

computeBaseFare :: Monad m => ServiceHandle m -> FareConfig -> DistanceInM -> BusinessRule m Float
computeBaseFare ServiceHandle {..} _fareConfig _actualDistance = pure 0

computeAdditionalFare :: Monad m => ServiceHandle m -> FareConfig -> TripStartTime -> BusinessRule m Float
computeAdditionalFare ServiceHandle {..} _fareConfig _startTime = pure 0
