module Product.FareCalculator.Interpreter
  ( FareParameters (..),
    RentalFareParameters (..),
    calculateFare,
    calculateRentalFare,
    fareSum,
    fareSumWithDiscount,
    rentalFareSum,
    rentalFareSumWithDiscount,
  )
where

import Beckn.Types.Amount (Amount)
import Beckn.Types.Id
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import Product.FareCalculator.Flow
  ( FareParameters (..),
    RentalFareParameters (..),
    ServiceHandle (..),
    doCalculateFare,
    doCalculateRentalFare,
  )
import qualified Storage.Queries.FarePolicy as FarePolicyS
import qualified Storage.Queries.RentalFarePolicy as RentalFarePolicyS
import Tools.Metrics (CoreMetrics)
import Utils.Common

calculateFare ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id Organization ->
  Vehicle.Variant ->
  Meter ->
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

calculateRentalFare ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id Organization ->
  Vehicle.Variant ->
  m RentalFareParameters
calculateRentalFare orgId vehicleVariant = do
  logTagInfo "FareCalculator" $ "Initiating rental fare calculation for organization " +|| orgId ||+ " for " +|| vehicleVariant ||+ ""
  rentalFareParams <-
    doCalculateRentalFare
      serviceHandle
      orgId
      vehicleVariant
  logTagInfo
    "FareCalculator"
    $ "Rental fare parameters calculated: " +|| rentalFareParams ||+ ""
  pure rentalFareParams

serviceHandle ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = \orgId vehicleVariant -> do
        FarePolicyS.findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant,
      getRentalFarePolicy = \orgId vehicleVariant -> do
        RentalFarePolicyS.findRentalFarePolicyByOrgAndVehicleVariant orgId vehicleVariant
    }

fareSum :: FareParameters -> Amount
fareSum FareParameters {..} =
  nightShiftRate * (baseFare + distanceFare)

fareSumWithDiscount :: FareParameters -> Amount
fareSumWithDiscount fp@FareParameters {..} = do
  let fareSumm = fareSum fp
  max 0 $ maybe fareSumm (fareSumm -) discount

rentalFareSum :: RentalFareParameters -> Amount
rentalFareSum RentalFareParameters {..} = do
  let extraFare = sum . map (fromMaybe 0) $ [extraDistanceFare, extraDurationFare, forNextDaysFare]
  baseFare + extraFare

rentalFareSumWithDiscount :: RentalFareParameters -> Amount
rentalFareSumWithDiscount rfp@RentalFareParameters {..} = do
  let fareSumm = rentalFareSum rfp
  max 0 $ maybe fareSumm (fareSumm -) discount
