module FareCalculator where

import Beckn.Types.Amount
import Beckn.Types.App
import Beckn.Types.Id
import Data.Time hiding (parseTime)
import EulerHS.Prelude
import Product.FareCalculator.Flow
import Servant.Server
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Types.Domain.FarePolicy
import Types.Error
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.SearchReqLocation as Location
import qualified Types.Storage.Vehicle as Vehicle
import Utils.GuidGenerator ()
import Utils.SilentLogger ()
import Utils.Time

defaultFarePolicy :: FarePolicy
defaultFarePolicy =
  FarePolicy
    { id = "fare_config_id",
      vehicleVariant = Vehicle.HATCHBACK,
      organizationId = orgID,
      baseFare = Just 120.0,
      baseDistance = Just 5000.0,
      perExtraKmRateList = [defaultExtraKmRate],
      nightShiftStart = Just midnight,
      nightShiftEnd = Just midnight,
      nightShiftRate = Just 1.0
    }

defaultPickupLocation :: PickupLocation
defaultPickupLocation =
  PickupLocation $
    Location.SearchReqLocation
      { id = Id "",
        lat = 0.0,
        long = 0.0,
        district = Nothing,
        city = Nothing,
        state = Nothing,
        country = Nothing,
        pincode = Nothing,
        address = Nothing,
        createdAt = mockTime,
        updatedAt = mockTime
      }

defaultDropLocation :: DropLocation
defaultDropLocation =
  DropLocation $
    Location.SearchReqLocation
      { id = Id "",
        lat = 0.0,
        long = 0.0,
        district = Nothing,
        city = Nothing,
        state = Nothing,
        country = Nothing,
        pincode = Nothing,
        address = Nothing,
        createdAt = mockTime,
        updatedAt = mockTime
      }

mockTime :: UTCTime
mockTime = parseTime "2018-12-06T11:39:57.153Z"

orgID :: Id Organization.Organization
orgID = "organization_id"

handle :: ServiceHandle IO
handle =
  ServiceHandle
    { getFarePolicy = \orgId vehicleVariant -> pure $ Just defaultFarePolicy,
      getDistance = \pickup drop -> pure $ Just 0
    }

-- Calculation tests

hatchback20km :: TestTree
hatchback20km = testCase "Calculate fare for 20km with FullReturnTrip for Hatchback" $ do
  fareParams <-
    doCalculateFare
      handle
      orgID
      Vehicle.HATCHBACK
      (Right distance)
      FullReturnTrip
      startTime
  let totalFare = fareSum fareParams
  totalFare @?= Amount 540.0
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = 20000.0

sedan20km :: TestTree
sedan20km = testCase "Calculate fare for 20km with FullReturnTrip for Sedan" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SEDAN
      (Right distance)
      FullReturnTrip
      startTime
  let totalFare = fareSum fareParams
  totalFare @?= Amount 675.0
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = 20000.0
    handle' =
      handle
        { getFarePolicy = \_orgId vehicleVariant ->
            pure $
              Just
                defaultFarePolicy
                  { vehicleVariant = Vehicle.SEDAN,
                    baseFare = Just 150.0,
                    perExtraKmRateList =
                      [ defaultExtraKmRate{fromExtraDistance = 10000, extraFare = 15},
                        defaultExtraKmRate{fromExtraDistance = 20000, extraFare = 25}
                      ]
                  }
        }

suv20km :: TestTree
suv20km = testCase "Calculate fare for 20km with FullReturnTrip for SUV" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SUV
      (Right distance)
      FullReturnTrip
      startTime
  let totalFare = fareSum fareParams
  totalFare @?= Amount 800.0
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = 20000.0
    handle' =
      handle
        { getFarePolicy = \_orgId vehicleVariant ->
            pure $
              Just
                defaultFarePolicy
                  { vehicleVariant = Vehicle.SUV,
                    baseFare = Just 0,
                    baseDistance = Just 0,
                    perExtraKmRateList =
                      [ defaultExtraKmRate,
                        defaultExtraKmRate{fromExtraDistance = 10000, extraFare = 20},
                        defaultExtraKmRate{fromExtraDistance = 20000, extraFare = 25}
                      ]
                  }
        }

-- Night Shift

nightHatchback20km :: TestTree
nightHatchback20km = testCase "Calculate night shift fare for 20km with OneWayTrip for Hatchback at 21:00" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.HATCHBACK
      (Right distance)
      OneWayTrip
      startTime
  let totalFare = fareSum fareParams
  totalFare @?= Amount 347.6
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = 20000.0
    handle' =
      handle
        { getFarePolicy = \_orgId vehicleVariant ->
            pure $
              Just
                defaultFarePolicy
                  { vehicleVariant = Vehicle.HATCHBACK,
                    baseFare = Just 100.0,
                    baseDistance = Just 4000.0,
                    perExtraKmRateList =
                      [ defaultExtraKmRate,
                        defaultExtraKmRate{fromExtraDistance = 10000, extraFare = 13.5},
                        defaultExtraKmRate{fromExtraDistance = 20000, extraFare = 20}
                      ],
                    nightShiftStart = Just $ TimeOfDay 20 0 0,
                    nightShiftEnd = Just $ TimeOfDay 5 30 0,
                    nightShiftRate = Just 1.1
                  }
        }

nightSedan20km :: TestTree
nightSedan20km = testCase "Calculate night shift fare for 20km with OneWayTrip for Sedan" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SEDAN
      (Right distance)
      OneWayTrip
      startTime
  let totalFare = fareSum fareParams
  totalFare @?= Amount 390.5
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = 20000.0
    handle' =
      handle
        { getFarePolicy = \_orgId vehicleVariant ->
            pure $
              Just
                defaultFarePolicy
                  { vehicleVariant = Vehicle.SEDAN,
                    baseFare = Just 100.0,
                    baseDistance = Just 3000.0,
                    perExtraKmRateList =
                      [ defaultExtraKmRate,
                        defaultExtraKmRate{fromExtraDistance = 10000, extraFare = 15},
                        defaultExtraKmRate{fromExtraDistance = 20000, extraFare = 18}
                      ],
                    nightShiftStart = Just $ TimeOfDay 20 0 0,
                    nightShiftEnd = Just $ TimeOfDay 5 30 0,
                    nightShiftRate = Just 1.1
                  }
        }

nightSuv20km :: TestTree
nightSuv20km = testCase "Calculate night shift fare for 20km with OneWayTrip for SUV" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SUV
      (Right distance)
      OneWayTrip
      startTime
  let totalFare = fareSum fareParams
  totalFare @?= Amount 539.0
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = 20000.0
    handle' =
      handle
        { getFarePolicy = \_orgId vehicleVariant ->
            pure $
              Just
                defaultFarePolicy
                  { vehicleVariant = Vehicle.SUV,
                    baseFare = Just 150.0,
                    baseDistance = Just 3000.0,
                    perExtraKmRateList =
                      [ defaultExtraKmRate,
                        defaultExtraKmRate{fromExtraDistance = 10000, extraFare = 20},
                        defaultExtraKmRate{fromExtraDistance = 20000, extraFare = 25}
                      ],
                    nightShiftStart = Just $ TimeOfDay 20 0 0,
                    nightShiftEnd = Just $ TimeOfDay 5 30 0,
                    nightShiftRate = Just 1.1
                  }
        }

-- Effects tests

failOnMissingFareConfig :: TestTree
failOnMissingFareConfig = testCase "Fail on missing FarePolicy" $ do
  doCalculateFare
    handle'
    orgID
    Vehicle.SEDAN
    (Right distance)
    OneWayTrip
    startTime
    `shouldThrow` (== NoFarePolicy)
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = 0.0
    handle' =
      handle
        { getFarePolicy = \_orgId vehicleVariant -> pure Nothing
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
