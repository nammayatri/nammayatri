{-# LANGUAGE TypeApplications #-}

module Product.FareCalculator.Interpreter (calculateFare) where

import App.Types (Flow)
import Beckn.Product.BusinessRule (runBRFlowMaybe)
import Beckn.Types.Amount (Amount)
import Beckn.Types.App (OrganizationId (_getOrganizationId))
import Beckn.Types.ID (ID (ID))
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
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
import qualified Types.Storage.FarePolicy as FarePolicyS

calculateFare ::
  OrganizationId ->
  Vehicle.Variant ->
  Location.Location ->
  Location.Location ->
  UTCTime ->
  Maybe Text ->
  Flow (Maybe Amount)
calculateFare orgId vehicleVariant pickLoc dropLoc startTime mbDistance = do
  L.logInfo @Text "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ " for " +|| vehicleVariant ||+ ""
  fareParams <-
    runBRFlowMaybe $
      doCalculateFare
        serviceHandle
        (ID $ _getOrganizationId orgId)
        vehicleVariant
        (PickupLocation pickLoc)
        (DropLocation dropLoc)
        OneWayTrip -- TODO :: determine the type of trip
        startTime
        (mbDistance >>= readMaybe . T.unpack)
  let totalFare = fareSum <$> fareParams
  L.logInfo @Text
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ". Total fare: " +|| totalFare ||+ ""
  pure totalFare

serviceHandle :: ServiceHandle Flow
serviceHandle =
  ServiceHandle
    { getFarePolicy = \orgId vehicleVariant -> do
        sFarePolicy <- lift $ FarePolicyS.findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant
        let farePolicy = FarePolicyS.fromTable <$> sFarePolicy
        pure farePolicy,
      getDistance = \(PickupLocation pickupLoc) (DropLocation dropLoc) ->
        lift $ Location.calculateDistance pickupLoc dropLoc
    }
