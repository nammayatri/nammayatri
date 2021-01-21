module FareCalculator where

import Beckn.Types.Amount
import Beckn.Types.App
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time hiding (parseTime)
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
      vehicleType = Vehicle.HATCHBACK,
      organizationId = orgID,
      baseFare = Just 120.0,
      baseDistance = Just 5000.0,
      perExtraKmRate = 12.0,
      perDeadKmRate = 12.0,
      minDeadKmThreshold = Just 5000.0,
      nightShiftStart = midnight,
      nightShiftEnd = midnight,
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

hatchback20km :: TestTree
hatchback20km = testCase "Calculate fare for 20km with FullReturnTrip for Hatchback" $ do
  fareParams <-
    runBR $
      calculateFare
        handle
        orgID
        Vehicle.HATCHBACK
        defaultPickupLocation
        defaultDropLocation
        FullReturnTrip
        startTime
        distance
        deadDistance
  let totalFare = fareSum <$> fareParams
  totalFare @?= Right (Amount 540.0)
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = Just 20000.0
    deadDistance = 5000.0

sedan20km :: TestTree
sedan20km = testCase "Calculate fare for 20km with FullReturnTrip for Sedan" $ do
  fareParams <-
    runBR $
      calculateFare
        handle'
        orgID
        Vehicle.SEDAN
        defaultPickupLocation
        defaultDropLocation
        FullReturnTrip
        startTime
        distance
        deadDistance
  let totalFare = fareSum <$> fareParams
  totalFare @?= Right (Amount 675.0)
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = Just 20000.0
    deadDistance = 5000.0
    handle' =
      handle
        { getFareConfig = \_orgId _vehicleType ->
            pure $
              Just
                defaultFareConfig
                  { vehicleType = Vehicle.SEDAN,
                    baseFare = Just 150.0,
                    perExtraKmRate = 15.0,
                    perDeadKmRate = 15.0
                  }
        }

suv20km :: TestTree
suv20km = testCase "Calculate fare for 20km with FullReturnTrip for SUV" $ do
  fareParams <-
    runBR $
      calculateFare
        handle'
        orgID
        Vehicle.SUV
        defaultPickupLocation
        defaultDropLocation
        FullReturnTrip
        startTime
        distance
        deadDistance
  let totalFare = fareSum <$> fareParams
  totalFare @?= Right (Amount 800.0)
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = Just 20000.0
    deadDistance = 5000.0
    handle' =
      handle
        { getFareConfig = \_orgId _vehicleType ->
            pure $
              Just
                defaultFareConfig
                  { vehicleType = Vehicle.SUV,
                    baseFare = Just 0,
                    baseDistance = Just 0,
                    perExtraKmRate = 20.0,
                    perDeadKmRate = 20.0
                  }
        }

-- Night Shift

nightHatchback20km :: TestTree
nightHatchback20km = testCase "Calculate night shift fare for 20km with OneWayTrip for Hatchback at 21:00" $ do
  fareParams <-
    runBR $
      calculateFare
        handle'
        orgID
        Vehicle.HATCHBACK
        defaultPickupLocation
        defaultDropLocation
        OneWayTrip
        startTime
        distance
        deadDistance
  let totalFare = fareSum <$> fareParams
  totalFare @?= Right (Amount 407.0)
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = Just 20000.0
    deadDistance = 5000.0
    handle' =
      handle
        { getFareConfig = \_orgId _vehicleType ->
            pure $
              Just
                defaultFareConfig
                  { vehicleType = Vehicle.HATCHBACK,
                    baseFare = Just 100.0,
                    baseDistance = Just 4000.0,
                    perExtraKmRate = 13.5,
                    perDeadKmRate = 13.5,
                    minDeadKmThreshold = Just 1000,
                    nightShiftStart = TimeOfDay 20 0 0,
                    nightShiftEnd = TimeOfDay 5 30 0,
                    nightShiftRate = 1.1
                  }
        }

nightSedan20km :: TestTree
nightSedan20km = testCase "Calculate night shift fare for 20km with OneWayTrip for Sedan" $ do
  fareParams <-
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
  let totalFare = fareSum <$> fareParams
  totalFare @?= Right (Amount 456.5)
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = Just 20000.0
    deadDistance = 5000.0
    handle' =
      handle
        { getFareConfig = \_orgId _vehicleType ->
            pure $
              Just
                defaultFareConfig
                  { vehicleType = Vehicle.SEDAN,
                    baseFare = Just 100.0,
                    baseDistance = Just 3000.0,
                    perExtraKmRate = 15.0,
                    perDeadKmRate = 15.0,
                    minDeadKmThreshold = Just 1000,
                    nightShiftStart = TimeOfDay 20 0 0,
                    nightShiftEnd = TimeOfDay 5 30 0,
                    nightShiftRate = 1.1
                  }
        }

nightSuv20km :: TestTree
nightSuv20km = testCase "Calculate night shift fare for 20km with OneWayTrip for SUV" $ do
  fareParams <-
    runBR $
      calculateFare
        handle'
        orgID
        Vehicle.SUV
        defaultPickupLocation
        defaultDropLocation
        OneWayTrip
        startTime
        distance
        deadDistance
  let totalFare = fareSum <$> fareParams
  totalFare @?= Right (Amount 627.0)
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = Just 20000.0
    deadDistance = 5000.0
    handle' =
      handle
        { getFareConfig = \_orgId _vehicleType ->
            pure $
              Just
                defaultFareConfig
                  { vehicleType = Vehicle.SUV,
                    baseFare = Just 150.0,
                    baseDistance = Just 3000.0,
                    perExtraKmRate = 20.0,
                    perDeadKmRate = 20.0,
                    minDeadKmThreshold = Just 1000,
                    nightShiftStart = TimeOfDay 20 0 0,
                    nightShiftEnd = TimeOfDay 5 30 0,
                    nightShiftRate = 1.1
                  }
        }

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
    startTime = parseTime "2018-12-06T21:00:00.000Z"
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
    [ hatchback20km,
      sedan20km,
      suv20km,
      nightHatchback20km,
      nightSedan20km,
      nightSuv20km,
      failOnMissingFareConfig
    ]
