module FareCalculator where

import Beckn.Types.Amount
import Beckn.Types.App
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time (UTCTime)
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
    { id = "fare_config_id",
      vehicleType = Vehicle.SEDAN,
      organizationId = orgID,
      baseFare = Just 120.0,
      baseDistance = Just 5000.0,
      perExtraKmRate = 12.0,
      perDeadKmRate = 12.0,
      minDeadKmThreshold = Just 5000.0,
      nightShiftStart = undefined,
      nightShiftEnd = undefined,
      nightShiftRate = 1.0
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

mockTime :: UTCTime
mockTime = parseTime "2018-12-06T11:39:57.153Z"

orgID :: ID Organization.Organization
orgID = "organization_id"

handle :: ServiceHandle IO
handle =
  ServiceHandle
    { getFareConfig = \orgId vehicleType -> pure $ Just defaultFareConfig,
      getDistance = \pickup drop -> pure 0
    }

-- Calculation tests

calculate20KmFare :: TestTree
calculate20KmFare = testCase "Calculate fare for 20km with FullReturnTrip" $ do
  fareParams <-
    runBR $
      calculateFare
        handle
        orgID
        Vehicle.SEDAN
        defaultPickupLocation
        defaultDropLocation
        FullReturnTrip
        startTime
        distance
        deadDistance
  let totalFare = fareSum <$> fareParams
  totalFare @?= Right (Amount 540.0)
  where
    startTime = parseTime "2018-12-06T11:39:57.153Z"
    distance = Just 20000.0
    deadDistance = 5000.0

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
        OneWayTrip
        startTime
        distance
        deadDistance
  result
    @?= Left
      ( BusinessError
          "NO_FARE_CONFIG"
          "FareConfig was not found."
      )
  where
    startTime = parseTime "2018-12-06T11:39:57.153Z"
    distance = Just 0.0
    deadDistance = 5000.0
    handle' =
      handle
        { getFareConfig = \_orgId _vehicleType -> pure Nothing
        }

fareCalculator :: TestTree
fareCalculator =
  testGroup
    "Fare Calculator"
    [ calculate20KmFare,
      failOnMissingFareConfig
    ]
