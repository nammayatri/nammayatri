module Product.FareCalculator.Interpreter (calculateFare) where

import App.Types (Flow)
import Beckn.Product.BusinessRule (runBRFlowFatal)
import Beckn.Types.Amount (Amount)
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Location as Location
import Beckn.Types.Storage.Organization (Organization)
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Beckn.Utils.Common
import qualified Data.Text as T
import Data.Time (UTCTime)
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
  Id Organization ->
  Vehicle.Variant ->
  Location.Location ->
  Location.Location ->
  UTCTime ->
  Maybe Text ->
  Flow Amount
calculateFare orgId vehicleVariant pickLoc dropLoc startTime mbDistance = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ " for " +|| vehicleVariant ||+ ""
  fareParams <-
    runBRFlowFatal $
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
