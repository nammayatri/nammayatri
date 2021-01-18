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

defaultFareConfig :: FareConfig
defaultFareConfig =
  FareConfig
    { id = ID "fare_config_id",
      vehicleType = Vehicle.SEDAN,
      organizationId = orgID,
      minimumFare = Just 90.0,
      perKmRate = 50.0,
      minimumDistance = Just 1000.0,
      multiplicationFactor = Nothing
    }

defaultPickupLocation :: PickupLocation
defaultPickupLocation =
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

defaultDropLocation :: DropLocation
defaultDropLocation =
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

mockTime = parseTime "2018-12-06T11:39:57.153Z"

orgID = ID "organization_id"

handle =
  ServiceHandle
    { getFareConfig = \orgId vehicleType -> pure $ Just defaultFareConfig,
      getDistance = \pickup drop -> pure 0
    }

-- Calculation tests

calculate5KmFare :: TestTree
calculate5KmFare = testCase "Calculate simple fare for 5km" $ do
  totalFare <-
    runBR $
      calculateFare
        handle
        orgID
        Vehicle.SEDAN
        defaultPickupLocation
        defaultDropLocation
        startTime
        distance
  totalFare @?= Right (Amount 250.0)
  where
    startTime = parseTime "2018-12-06T11:39:57.153Z"
    distance = Just 5000.0

calculate25KmFare :: TestTree
calculate25KmFare = testCase "Calculate fare for 25km" $ do
  totalFare <-
    runBR $
      calculateFare
        handle
        orgID
        Vehicle.SEDAN
        defaultPickupLocation
        defaultDropLocation
        startTime
        distance
  totalFare @?= Right (Amount 1250.0)
  where
    startTime = parseTime "2018-12-06T11:39:57.153Z"
    distance = Just 25000.0

calculate500MFare :: TestTree
calculate500MFare = testCase "Calculate fare with minimum distance for 500m" $ do
  totalFare <-
    runBR $
      calculateFare
        handle
        orgID
        Vehicle.SEDAN
        defaultPickupLocation
        defaultDropLocation
        startTime
        distance
  totalFare @?= Right (Amount 50)
  where
    startTime = parseTime "2018-12-06T11:39:57.153Z"
    distance = Just 500.0

-- Effects tests

failOnMissingFareConfig :: TestTree
failOnMissingFareConfig = testCase "Fail on missing FareConfig" $ do
  result <-
    runBR $
      calculateFare
        handle'
        orgID
        Vehicle.SEDAN
        defaultPickupLocation
        defaultDropLocation
        startTime
        distance
  result
    @?= Left
      ( BusinessError
          "NO_FARE_CONFIG"
          "No FareConfig found for ID \"organization_id\" with vehicle type SEDAN"
      )
  where
    startTime = parseTime "2018-12-06T11:39:57.153Z"
    distance = Just 0.0
    handle' =
      handle
        { getFareConfig = \_orgId _vehicleType -> pure Nothing
        }

fareCalculator :: TestTree
fareCalculator =
  testGroup
    "Fare Calculator"
    [ calculate25KmFare,
      calculate5KmFare,
      calculate500MFare,
      failOnMissingFareConfig
    ]
