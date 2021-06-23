module Product.FareCalculator.Interpreter (calculateFare) where

import Beckn.Types.Amount (Amount)
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Location as Location
import Beckn.Types.Storage.Organization (Organization)
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import qualified Data.Text as T
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
import Utils.Common

calculateFare ::
  ( HasFlowDBEnv m r,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id Organization ->
  Vehicle.Variant ->
  Location.Location ->
  Location.Location ->
  UTCTime ->
  Maybe Text ->
  m Amount
calculateFare orgId vehicleVariant pickLoc dropLoc startTime mbDistance = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ " for " +|| vehicleVariant ||+ ""
  fareParams <-
    doCalculateFare
      serviceHandle
      orgId
      vehicleVariant
      (PickupLocation pickLoc)
      (DropLocation dropLoc)
      OneWayTrip -- TODO :: determine the type of trip
      startTime
      (mbDistance >>= readMaybe . T.unpack)
  let totalFare = fareSum fareParams
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ". Total fare: " +|| totalFare ||+ ""
  pure totalFare

serviceHandle ::
  ( HasFlowDBEnv m r,
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
