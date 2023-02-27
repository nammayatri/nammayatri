{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module RentalFareCalculator where

import Data.Time hiding (parseTime)
import Domain.Types.FarePolicy.RentalFarePolicy
import EulerHS.Prelude
import qualified Fixtures.RentalFarePolicy as Fixtures
import Kernel.Prelude (roundToIntegral)
import Kernel.Types.Common
import Kernel.Types.Id
import SharedLogic.FareCalculator.RentalFareCalculator
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Tools.Error
import Utils.GuidGenerator ()
import Utils.SilentLogger ()
import Utils.Time

rentalFarePolicyId :: Id RentalFarePolicy
rentalFarePolicyId = Fixtures.defaultFarePolicy.id

handle :: ServiceHandle IO
handle =
  ServiceHandle
    { getRentalFarePolicy = \rentalFarePolicyId_ -> pure $ case rentalFarePolicyId_ of
        "rentalFarePolicyId" -> Just Fixtures.defaultFarePolicy
        _ -> Nothing
    }

-- FIXME can we provide mock data without parsing?
mockTime :: Int -> Int -> UTCTime
mockTime hours minutes =
  parseTime $
    "2018-12-06T"
      <> (if hours <= 9 then "0" else "")
      <> show hours
      <> ":"
      <> (if minutes <= 9 then "0" else "")
      <> show minutes
      <> ":00.000Z"

mockTime2 :: UTCTime
mockTime2 = parseTime "2018-12-09T00:00:00.000Z"

-- Calculation tests

onlyBaseFare :: TestTree
onlyBaseFare = testCase "Rental fare consist of only base fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= 120
  where
    startTime = mockTime 2 0
    stopTime = mockTime 4 30
    distance = Meters 90000

edgeCase :: TestTree
edgeCase = testCase "Edge case for rental fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= 120
  where
    startTime = mockTime 18 30
    stopTime = mockTime 21 30
    distance = Meters 100000

incorrectData :: TestTree
incorrectData = testCase "Incorrect data for rental fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= 120
  where
    startTime = mockTime 4 30
    stopTime = mockTime 2 0
    distance = Meters 90000

-- 120+7*2=134
extraDistance :: TestTree
extraDistance = testCase "Rental fare consist of base fare and extra distance fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= 134
  where
    startTime = mockTime 2 0
    stopTime = mockTime 4 30
    distance = Meters 107000

-- 120+1*40=160
extraTime :: TestTree
extraTime = testCase "Rental fare consist of base fare and extra time fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= 160
  where
    startTime = mockTime 2 0
    stopTime = mockTime 5 40
    distance = Meters 90000

-- using local time IST +0530, so this trip started at 23:30 and ended next day at 02:10
-- 120+1*30=150
nextDay :: TestTree
nextDay = testCase "Rental fare consist of base fare and next day fare" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= 150
  where
    startTime = mockTime 18 0
    stopTime = mockTime 20 40
    distance = Meters 90000

-- base fare: 120; extra distance fare: 15*2=30; extra time fare : (3*24*60-3*60)*1=4140; next days fare: 30*3=90; total fare: 4380
allFareParameters :: TestTree
allFareParameters = testCase "Rental fare consist of all fare parameters" $ do
  fareParams <- doCalculateRentalFare handle rentalFarePolicyId distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  totalFare @?= 4380
  where
    startTime = mockTime 0 0
    stopTime = mockTime2
    distance = Meters 115000

fareBreakupSum :: TestTree
fareBreakupSum = testCase "Sum of rental fare breakup should be equal to total fare" $ do
  fareParams <- doCalculateRentalFare handle "rentalFarePolicyId" distance startTime stopTime
  let totalFare = rentalFareSumWithDiscount fareParams
  fareBreakups <- buildRentalFareBreakups fareParams "bookingId"
  roundToIntegral (sum (fareBreakups <&> (.amount))) `shouldBe` totalFare
  where
    startTime = mockTime 15 13
    stopTime = mockTime2
    distance = Meters 116235

-- Effects tests

failOnMissingFareConfig :: TestTree
failOnMissingFareConfig = testCase "Fail on missing RentalFarePolicy" $ do
  doCalculateRentalFare handle "fakeRentalFarePolicyId" distance startTime stopTime `shouldThrow` (== NoRentalFarePolicy)
  where
    startTime = mockTime 2 0
    stopTime = mockTime 4 30
    distance = Meters 90000

rentalFareCalculator :: TestTree
rentalFareCalculator =
  testGroup
    "Rental Fare Calculator"
    [ onlyBaseFare,
      edgeCase,
      incorrectData,
      extraDistance,
      extraTime,
      nextDay,
      allFareParameters,
      fareBreakupSum,
      failOnMissingFareConfig
    ]
