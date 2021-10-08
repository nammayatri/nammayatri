module FareCalculator where

import Beckn.Types.Amount
import Beckn.Types.App
import Beckn.Types.Id
import Data.Time hiding (parseTime)
import EulerHS.Prelude
import Product.FareCalculator.Flow
import Product.FareCalculator.Interpreter
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

defaultPerExtraKmRate :: PerExtraKmRate
defaultPerExtraKmRate = PerExtraKmRate 5000 12.0

defaultFarePolicy :: FarePolicy
defaultFarePolicy =
  FarePolicy
    { id = "fare_config_id",
      vehicleVariant = Vehicle.HATCHBACK,
      organizationId = orgID,
      baseFare = Just 120.0,
      perExtraKmRateList = defaultPerExtraKmRate :| [],
      discountList = [],
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
    { getFarePolicy = \orgId vehicleVariant -> pure $ Just defaultFarePolicy
    }

-- Calculation tests

hatchback20km :: TestTree
hatchback20km = testCase "Calculate fare for 20km for Hatchback" $ do
  fareParams <-
    doCalculateFare
      handle
      orgID
      Vehicle.HATCHBACK
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= Amount 300.0
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = 20000.0

sedan10km :: TestTree
sedan10km = testCase "Calculate fare for 10km for Sedan" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SEDAN
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= Amount 250.0
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = 10000.0
    handle' =
      handle
        { getFarePolicy = \_orgId vehicleVariant ->
            pure $
              Just
                defaultFarePolicy
                  { vehicleVariant = Vehicle.SEDAN,
                    baseFare = Just 175.0,
                    perExtraKmRateList =
                      defaultPerExtraKmRate{distanceRangeStart = 5000, fare = 15}
                        :| [ defaultPerExtraKmRate{distanceRangeStart = 15000, fare = 30}
                           ]
                  }
        }

sedan20km :: TestTree
sedan20km = testCase "Calculate fare for 20km for Sedan" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SEDAN
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= Amount 475.0
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
                    baseFare = Just 175.0,
                    perExtraKmRateList =
                      defaultPerExtraKmRate{distanceRangeStart = 5000, fare = 15}
                        :| [ defaultPerExtraKmRate{distanceRangeStart = 15000, fare = 30}
                           ]
                  }
        }

sedan30km :: TestTree
sedan30km = testCase "Calculate fare for 30km for Sedan" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SEDAN
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= Amount 775.0
  where
    startTime = parseTime "2018-12-06T21:00:00.000Z"
    distance = 30000.0
    handle' =
      handle
        { getFarePolicy = \_orgId vehicleVariant ->
            pure $
              Just
                defaultFarePolicy
                  { vehicleVariant = Vehicle.SEDAN,
                    baseFare = Just 175.0,
                    perExtraKmRateList =
                      defaultPerExtraKmRate{distanceRangeStart = 5000, fare = 15}
                        :| [ defaultPerExtraKmRate{distanceRangeStart = 15000, fare = 30}
                           ]
                  }
        }

suv20km :: TestTree
suv20km = testCase "Calculate fare for 20km for SUV" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SUV
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= Amount 320.0
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
                    perExtraKmRateList =
                      defaultPerExtraKmRate{distanceRangeStart = 0}
                        :| [ defaultPerExtraKmRate{distanceRangeStart = 10000, fare = 20},
                             defaultPerExtraKmRate{distanceRangeStart = 20000, fare = 25}
                           ]
                  }
        }

-- Night Shift

nightHatchback20km :: TestTree
nightHatchback20km = testCase "Calculate night shift fare for 20km for Hatchback at 21:00" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.HATCHBACK
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= Amount 331.1
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
                    perExtraKmRateList =
                      defaultPerExtraKmRate{distanceRangeStart = 4000}
                        :| [ defaultPerExtraKmRate{distanceRangeStart = 14000, fare = 13.5},
                             defaultPerExtraKmRate{distanceRangeStart = 24000, fare = 20}
                           ],
                    nightShiftStart = Just $ TimeOfDay 20 0 0,
                    nightShiftEnd = Just $ TimeOfDay 5 30 0,
                    nightShiftRate = Just 1.1
                  }
        }

nightSedan20km :: TestTree
nightSedan20km = testCase "Calculate night shift fare for 20km for Sedan" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SEDAN
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= Amount 357.5
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
                    perExtraKmRateList =
                      defaultPerExtraKmRate{distanceRangeStart = 3000}
                        :| [ defaultPerExtraKmRate{distanceRangeStart = 13000, fare = 15},
                             defaultPerExtraKmRate{distanceRangeStart = 23000, fare = 18}
                           ],
                    nightShiftStart = Just $ TimeOfDay 20 0 0,
                    nightShiftEnd = Just $ TimeOfDay 5 30 0,
                    nightShiftRate = Just 1.1
                  }
        }

nightSuv20km :: TestTree
nightSuv20km = testCase "Calculate night shift fare for 20km for SUV" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SUV
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= Amount 451.0
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
                    perExtraKmRateList =
                      defaultPerExtraKmRate{distanceRangeStart = 3000}
                        :| [ defaultPerExtraKmRate{distanceRangeStart = 13000, fare = 20},
                             defaultPerExtraKmRate{distanceRangeStart = 23000, fare = 25}
                           ],
                    nightShiftStart = Just $ TimeOfDay 20 0 0,
                    nightShiftEnd = Just $ TimeOfDay 5 30 0,
                    nightShiftRate = Just 1.1
                  }
        }

nightSuv20kmWithDiscount :: TestTree
nightSuv20kmWithDiscount = testCase "Calculate night shift fare for 20km for SUV with discount" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SUV
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= Amount 401.0
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
                    perExtraKmRateList =
                      defaultPerExtraKmRate{distanceRangeStart = 3000}
                        :| [ defaultPerExtraKmRate{distanceRangeStart = 13000, fare = 20},
                             defaultPerExtraKmRate{distanceRangeStart = 23000, fare = 25}
                           ],
                    discountList = [Discount midnight midday 50 True, Discount midday midnight 50 True],
                    nightShiftStart = Just $ TimeOfDay 20 0 0,
                    nightShiftEnd = Just $ TimeOfDay 5 30 0,
                    nightShiftRate = Just 1.1
                  }
        }

nightSuv20kmWithDiscountOff :: TestTree
nightSuv20kmWithDiscountOff = testCase "Calculate night shift fare for 20km for SUV with discount off" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SUV
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= Amount 451.0
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
                    perExtraKmRateList =
                      defaultPerExtraKmRate{distanceRangeStart = 3000}
                        :| [ defaultPerExtraKmRate{distanceRangeStart = 13000, fare = 20},
                             defaultPerExtraKmRate{distanceRangeStart = 23000, fare = 25}
                           ],
                    discountList = [Discount midnight midday 50 False],
                    nightShiftStart = Just $ TimeOfDay 20 0 0,
                    nightShiftEnd = Just $ TimeOfDay 5 30 0,
                    nightShiftRate = Just 1.1
                  }
        }

nightSuv20kmWithClashedDiscounts :: TestTree
nightSuv20kmWithClashedDiscounts = testCase "Calculate night shift fare for 20km for SUV with clashed discounts" $ do
  fareParams <-
    doCalculateFare
      handle'
      orgID
      Vehicle.SUV
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= Amount 351.0
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
                    perExtraKmRateList =
                      defaultPerExtraKmRate{distanceRangeStart = 3000}
                        :| [ defaultPerExtraKmRate{distanceRangeStart = 13000, fare = 20},
                             defaultPerExtraKmRate{distanceRangeStart = 23000, fare = 25}
                           ],
                    discountList = [Discount midnight midday 50 True, Discount midnight midday 50 True],
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
    distance
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
      sedan10km,
      sedan20km,
      sedan30km,
      suv20km,
      nightHatchback20km,
      nightSedan20km,
      nightSuv20km,
      nightSuv20kmWithDiscount,
      nightSuv20kmWithDiscountOff,
      nightSuv20kmWithClashedDiscounts,
      failOnMissingFareConfig
    ]
