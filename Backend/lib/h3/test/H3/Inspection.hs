{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Inspection (inspectionTests) where

import Control.Monad
import qualified H3.Functions.Indexing as H3
import qualified H3.Functions.Inspection as H3
import qualified H3.Functions.Miscellaneous as H3
import qualified H3.Functions.Types as H3
import Test.Hspec.Expectations
import Test.Tasty
import Test.Tasty.HUnit

inspectionTests :: TestTree
inspectionTests =
  testGroup
    "Inspection"
    [ getResolutionTest,
      getBaseCellNumberTest,
      h3ToStringTest,
      stringToH3Test,
      isValidCellTest,
      isResClassIIITest,
      isPentagonTest,
      getIcosahedronFacesTest
    ]

getResolutionTest :: TestTree
getResolutionTest = testCase "getResolution" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  H3.getResolution cell `shouldBe` 10

getBaseCellNumberTest :: TestTree
getBaseCellNumberTest = testCase "getBaseCellNumber" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  let cellNum = H3.getBaseCellNumber cell
  return $ cellNum `seq` ()

h3ToStringTest :: TestTree
h3ToStringTest = testCase "h3ToString" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  void . H3.h3ToString $ fromIntegral cell

stringToH3Test :: TestTree
stringToH3Test = testCase "stringToH3" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  strCell <- H3.h3ToString $ fromIntegral cell
  cell1 <- fromIntegral <$> H3.stringToH3 strCell
  cell `shouldBe` cell1

isValidCellTest :: TestTree
isValidCellTest = testCase "isValidCell" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  strCell <- H3.h3ToString $ fromIntegral cell
  cell1 <- fromIntegral <$> H3.stringToH3 strCell
  H3.isValidCell cell1 `shouldBe` True

isResClassIIITest :: TestTree
isResClassIIITest = testCase "isResClassIII" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  strCell <- H3.h3ToString $ fromIntegral cell
  cell1 <- fromIntegral <$> H3.stringToH3 strCell
  H3.isResClassIII cell1 `shouldBe` False

isPentagonTest :: TestTree
isPentagonTest = testCase "isPentagon" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  strCell <- H3.h3ToString $ fromIntegral cell
  cell1 <- fromIntegral <$> H3.stringToH3 strCell
  H3.isPentagon cell1 `shouldBe` False

getIcosahedronFacesTest :: TestTree
getIcosahedronFacesTest = testCase "getIcosahedronFaces" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  void $ H3.getIcosahedronFaces cell
