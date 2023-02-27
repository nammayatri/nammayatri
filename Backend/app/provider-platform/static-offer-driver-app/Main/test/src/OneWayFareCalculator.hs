{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module OneWayFareCalculator where

import Data.Time hiding (parseTime)
import Domain.Types.FarePolicy.Discount
import Domain.Types.FarePolicy.FareProduct
import Domain.Types.FarePolicy.OneWayFarePolicy
import Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude
import Kernel.Prelude (roundToIntegral)
import Kernel.Types.Common
import Kernel.Types.Id
import SharedLogic.FareCalculator.OneWayFareCalculator
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Tools.Error
import Utils.GuidGenerator ()
import Utils.SilentLogger ()
import Utils.Time

defaultPerExtraKmRate :: PerExtraKmRate
defaultPerExtraKmRate = PerExtraKmRate 5000 12.0

defaultOneWayFarePolicy :: OneWayFarePolicy
defaultOneWayFarePolicy =
  OneWayFarePolicy
    { id = "fare_config_id",
      vehicleVariant = Vehicle.HATCHBACK,
      merchantId = merchId,
      baseFare = Just 120,
      perExtraKmRateList = defaultPerExtraKmRate :| [],
      discountList = [],
      nightShiftStart = Just midnight,
      nightShiftEnd = Just midnight,
      nightShiftRate = Just 1.0,
      waitingChargePerMin = Just 1,
      createdAt = mockTime 0,
      updatedAt = mockTime 0
    }

mkDiscount :: Vehicle.Variant -> UTCTime -> UTCTime -> Money -> Bool -> Discount
mkDiscount vehVar from to disc isOn = Discount (Id "") vehVar (Id "") ONE_WAY from to disc isOn (mockTime 11) (mockTime 11)

mockTime :: Int -> UTCTime
mockTime hour = parseTime ("2018-12-06T" <> (if hour <= 9 then "0" else "") <> show hour <> ":00:00.000Z")

merchId :: Id DM.Merchant
merchId = "merchant_id"

handle :: ServiceHandle IO
handle =
  ServiceHandle
    { getFarePolicy = \_merchId _vehicleVariant -> pure $ Just defaultOneWayFarePolicy
    }

-- Calculation tests

hatchback20km :: TestTree
hatchback20km = testCase "Calculate fare for 20km for Hatchback" $ do
  fareParams <-
    doCalculateFare
      handle
      merchId
      Vehicle.HATCHBACK
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= 300
  where
    startTime = mockTime 2
    distance = Meters 20000

sedan10km :: TestTree
sedan10km = testCase "Calculate fare for 10km for Sedan" $ do
  fareParams <-
    doCalculateFare
      handle'
      merchId
      Vehicle.SEDAN
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= 250
  where
    startTime = mockTime 2
    distance = Meters 10000
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant ->
            pure $
              Just
                defaultOneWayFarePolicy{vehicleVariant = Vehicle.SEDAN,
                                        baseFare = Just 175,
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
      merchId
      Vehicle.SEDAN
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= 475
  where
    startTime = mockTime 2
    distance = Meters 20000
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant ->
            pure $
              Just
                defaultOneWayFarePolicy{vehicleVariant = Vehicle.SEDAN,
                                        baseFare = Just 175,
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
      merchId
      Vehicle.SEDAN
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= 775
  where
    startTime = mockTime 2
    distance = Meters 30000
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant ->
            pure $
              Just
                defaultOneWayFarePolicy{vehicleVariant = Vehicle.SEDAN,
                                        baseFare = Just 175,
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
      merchId
      Vehicle.SUV
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= 320
  where
    startTime = mockTime 2
    distance = Meters 20000
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant ->
            pure $
              Just
                defaultOneWayFarePolicy{vehicleVariant = Vehicle.SUV,
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
      merchId
      Vehicle.HATCHBACK
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= 331
  where
    startTime = mockTime 21
    distance = Meters 20000
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant ->
            pure $
              Just
                defaultOneWayFarePolicy{vehicleVariant = Vehicle.HATCHBACK,
                                        baseFare = Just 100,
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
      merchId
      Vehicle.SEDAN
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= 358
  where
    startTime = mockTime 21
    distance = Meters 20000
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant ->
            pure $
              Just
                defaultOneWayFarePolicy{vehicleVariant = Vehicle.SEDAN,
                                        baseFare = Just 100,
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
      merchId
      Vehicle.SUV
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= 451
  where
    startTime = mockTime 21
    distance = Meters 20000
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant ->
            pure $
              Just
                defaultOneWayFarePolicy{vehicleVariant = Vehicle.SUV,
                                        baseFare = Just 150,
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
      merchId
      Vehicle.SUV
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= 401
  where
    startTime = mockTime 21
    distance = Meters 20000
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant ->
            pure $
              Just
                defaultOneWayFarePolicy{vehicleVariant = Vehicle.SUV,
                                        baseFare = Just 150,
                                        perExtraKmRateList =
                                          defaultPerExtraKmRate{distanceRangeStart = 3000}
                                            :| [ defaultPerExtraKmRate{distanceRangeStart = 13000, fare = 20},
                                                 defaultPerExtraKmRate{distanceRangeStart = 23000, fare = 25}
                                               ],
                                        discountList =
                                          [ mkDiscount Vehicle.SUV (mockTime 19) (mockTime 20) 50 True,
                                            mkDiscount Vehicle.SUV (mockTime 20) (mockTime 23) 50 True
                                          ],
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
      merchId
      Vehicle.SUV
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= 451
  where
    startTime = mockTime 21
    distance = Meters 20000
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant ->
            pure $
              Just
                defaultOneWayFarePolicy{vehicleVariant = Vehicle.SUV,
                                        baseFare = Just 150,
                                        perExtraKmRateList =
                                          defaultPerExtraKmRate{distanceRangeStart = 3000}
                                            :| [ defaultPerExtraKmRate{distanceRangeStart = 13000, fare = 20},
                                                 defaultPerExtraKmRate{distanceRangeStart = 23000, fare = 25}
                                               ],
                                        discountList = [mkDiscount Vehicle.SUV (mockTime 0) (mockTime 23) 50 False],
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
      merchId
      Vehicle.SUV
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  totalFare @?= 351
  where
    startTime = mockTime 21
    distance = Meters 20000
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant ->
            pure $
              Just
                defaultOneWayFarePolicy{vehicleVariant = Vehicle.SUV,
                                        baseFare = Just 150,
                                        perExtraKmRateList =
                                          defaultPerExtraKmRate{distanceRangeStart = 3000}
                                            :| [ defaultPerExtraKmRate{distanceRangeStart = 13000, fare = 20},
                                                 defaultPerExtraKmRate{distanceRangeStart = 23000, fare = 25}
                                               ],
                                        discountList =
                                          [ mkDiscount Vehicle.SUV (mockTime 0) (mockTime 23) 50 True,
                                            mkDiscount Vehicle.SUV (mockTime 0) (mockTime 23) 50 True
                                          ],
                                        nightShiftStart = Just $ TimeOfDay 20 0 0,
                                        nightShiftEnd = Just $ TimeOfDay 5 30 0,
                                        nightShiftRate = Just 1.1
                                       }
        }

fareBreakupSum :: TestTree
fareBreakupSum = testCase "Sum of fare breakup should be equal to total fare" $ do
  fareParams <-
    doCalculateFare
      handle'
      merchId
      Vehicle.SUV
      distance
      startTime
  let totalFare = fareSumWithDiscount fareParams
  fareBreakups <- buildOneWayFareBreakups fareParams "bookingId"
  roundToIntegral (sum (fareBreakups <&> (.amount))) `shouldBe` totalFare
  where
    startTime = mockTime 19
    distance = Meters 18000
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant ->
            pure $
              Just
                defaultOneWayFarePolicy{vehicleVariant = Vehicle.SUV,
                                        baseFare = Just 120,
                                        perExtraKmRateList =
                                          defaultPerExtraKmRate{distanceRangeStart = 3100}
                                            :| [ defaultPerExtraKmRate{distanceRangeStart = 14000, fare = 21},
                                                 defaultPerExtraKmRate{distanceRangeStart = 24000, fare = 23}
                                               ],
                                        discountList =
                                          [ mkDiscount Vehicle.SUV (mockTime 2) (mockTime 23) 60 True,
                                            mkDiscount Vehicle.SUV (mockTime 2) (mockTime 23) 60 True
                                          ],
                                        nightShiftStart = Just $ TimeOfDay 21 0 0,
                                        nightShiftEnd = Just $ TimeOfDay 6 30 0,
                                        nightShiftRate = Just 1.15
                                       }
        }

-- Effects tests

failOnMissingFareConfig :: TestTree
failOnMissingFareConfig = testCase "Fail on missing FarePolicy" $ do
  doCalculateFare
    handle'
    merchId
    Vehicle.SEDAN
    distance
    startTime
    `shouldThrow` (== NoFarePolicy)
  where
    startTime = mockTime 21
    distance = Meters 0
    handle' =
      handle
        { getFarePolicy = \_merchId _vehicleVariant -> pure Nothing
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
      fareBreakupSum,
      failOnMissingFareConfig
    ]
