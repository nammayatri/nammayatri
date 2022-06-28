{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Indexing (indexingTests) where

import Control.Monad
import qualified H3.Functions.Indexing as H3
import qualified H3.Functions.Miscellaneous as H3
import qualified H3.Functions.Types as H3
import Test.Hspec.Expectations
import Test.Tasty
import Test.Tasty.HUnit

indexingTests :: TestTree
indexingTests =
  testGroup
    "Indexing"
    [ latLngToCellTest,
      cellToLatLngTest,
      cellToBoundaryTest
    ]

latLngToCellTest :: TestTree
latLngToCellTest = testCase "latLngToCell" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  void $ H3.latLngToCell latLong 10

cellToLatLngTest :: TestTree
cellToLatLngTest = testCase "cellToLatLng" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  void $ H3.cellToLatLng cell

cellToBoundaryTest :: TestTree
cellToBoundaryTest = testCase "cellToBoundary" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  void $ H3.cellToBoundary cell
