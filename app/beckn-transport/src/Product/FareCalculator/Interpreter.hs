module Product.FareCalculator.Interpreter
  ( FareParameters (..),
    calculateFare,
    fareSum,
    fareSumWithDiscount,
  )
where

import Beckn.Types.Amount (Amount)
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import Product.FareCalculator.Flow
  ( FareParameters (..),
    ServiceHandle (..),
    doCalculateFare,
  )
import qualified Storage.Queries.FarePolicy as FarePolicyS
import Types.Metrics (CoreMetrics)
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Vehicle as Vehicle
import Utils.Common

calculateFare ::
  ( DBFlow m r,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id Organization ->
  Vehicle.Variant ->
  Double ->
  UTCTime ->
  m FareParameters
calculateFare orgId vehicleVariant distance startTime = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ " for " +|| vehicleVariant ||+ ""
  fareParams <-
    doCalculateFare
      serviceHandle
      orgId
      vehicleVariant
      distance
      startTime
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams

serviceHandle ::
  ( DBFlow m r,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = \orgId vehicleVariant -> do
        FarePolicyS.findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant
    }

fareSum :: FareParameters -> Amount
fareSum FareParameters {..} =
  nightShiftRate * (baseFare + distanceFare)

fareSumWithDiscount :: FareParameters -> Amount
fareSumWithDiscount fp@FareParameters {..} = do
  let fareSumm = fareSum fp
  max 0 $ maybe fareSumm (fareSumm -) discount
