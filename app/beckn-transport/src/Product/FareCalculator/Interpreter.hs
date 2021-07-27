module Product.FareCalculator.Interpreter (calculateFare) where

import Beckn.Types.Amount (Amount)
import Beckn.Types.Id
import Control.Arrow (left, (***))
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Product.FareCalculator.Flow
  ( DropLocation (DropLocation),
    JourneyTrip (OneWayTrip),
    PickupLocation (PickupLocation),
    ServiceHandle (..),
    doCalculateFare,
    fareSum,
  )
import qualified Product.Location as Location
import qualified Storage.Queries.FarePolicy as FarePolicyS
import Types.Domain.FarePolicy
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.FarePolicy as FarePolicyS
import Types.Storage.Organization (Organization)
import qualified Types.Storage.SearchReqLocation as Location
import qualified Types.Storage.Vehicle as Vehicle
import Utils.Common

calculateFare ::
  ( DBFlow m r,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id Organization ->
  Vehicle.Variant ->
  Either
    ( Location.SearchReqLocation,
      Location.SearchReqLocation
    )
    Double ->
  UTCTime ->
  m Amount
calculateFare orgId vehicleVariant distanceSrc startTime = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ " for " +|| vehicleVariant ||+ ""
  fareParams <-
    doCalculateFare
      serviceHandle
      orgId
      vehicleVariant
      (distanceSrc & left (PickupLocation *** DropLocation))
      OneWayTrip -- TODO :: determine the type of trip
      startTime
  let totalFare = fareSum fareParams
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ". Total fare: " +|| totalFare ||+ ""
  pure totalFare

serviceHandle ::
  ( DBFlow m r,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = \orgId vehicleVariant -> do
        sFarePolicy <- FarePolicyS.findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant
        let farePolicy = fromTable <$> sFarePolicy
        pure farePolicy,
      getDistance = \(PickupLocation pickupLoc) (DropLocation dropLoc) ->
        Location.calculateDistance pickupLoc dropLoc
    }

fromTable :: FarePolicyS.FarePolicy -> FarePolicy
fromTable FarePolicyS.FarePolicy {..} =
  FarePolicy
    { id = id,
      vehicleVariant = vehicleVariant,
      organizationId = organizationId,
      baseFare = toRational <$> baseFare,
      baseDistance = toRational <$> baseDistance,
      perExtraKmRate = toRational perExtraKmRate,
      nightShiftStart = nightShiftStart,
      nightShiftEnd = nightShiftEnd,
      nightShiftRate = toRational <$> nightShiftRate
    }
