module FareCalculator where

import Beckn.Types.Amount
import Beckn.Types.App
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Beckn.Utils.Common
import EulerHS.Prelude
import Product.FareCalculator.BusinessRule
import Product.FareCalculator.Flow
import Product.FareCalculator.Models.FareConfig
import Product.FareCalculator.Models.ID
import Test.Tasty
import Test.Tasty.HUnit
import Utils.Time

calculateSimpleFare :: TestTree
calculateSimpleFare = testCase "Calculate simple Fare" $ do
  totalFare <- runBR $ calculateFare handle orgId vehicleType pickup drop startTime distance
  totalFare @?= Right (Amount 25.0)
  where
    orgId = ID "org_id"
    vehicleType = Vehicle.SEDAN
    fareConfig =
      FareConfig
        { id = ID "fare_config_id",
          vehicleType = vehicleType,
          organizationId = orgId,
          minimumFare = Just 10.0,
          perKmRate = 5.0,
          minimumDistance = Just 1000.0,
          multiplicationFactor = Nothing
        }
    pickup =
      PickupLocation $
        Location.Location
          { _id = LocationId "",
            _locationType = Location.POINT,
            _lat = Just 0.0,
            _long = Just 0.0,
            _ward = Nothing,
            _district = Nothing,
            _city = Nothing,
            _state = Nothing,
            _country = Nothing,
            _pincode = Nothing,
            _address = Nothing,
            _bound = Nothing,
            _createdAt = mockTime,
            _updatedAt = mockTime
          }
    drop =
      DropLocation $
        Location.Location
          { _id = LocationId "",
            _locationType = Location.POINT,
            _lat = Just 0.0,
            _long = Just 0.0,
            _ward = Nothing,
            _district = Nothing,
            _city = Nothing,
            _state = Nothing,
            _country = Nothing,
            _pincode = Nothing,
            _address = Nothing,
            _bound = Nothing,
            _createdAt = mockTime,
            _updatedAt = mockTime
          }
    startTime = parseTime "2018-12-06T11:39:57.153Z"
    mockTime = parseTime "2018-12-06T11:39:57.153Z"
    distance = Just 5000.0
    handle =
      ServiceHandle
        { getFareConfig = \orgId vehicleType -> pure $ Just fareConfig,
          getDistance = \pickup drop -> pure 0
        }

fareCalculator :: TestTree
fareCalculator = testGroup "Fare Calculator" [calculateSimpleFare]
