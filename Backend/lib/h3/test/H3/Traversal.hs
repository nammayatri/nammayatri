{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Traversal (traversalTests) where

import Control.Monad
import qualified H3.Functions.Indexing as H3
import qualified H3.Functions.Miscellaneous as H3
import qualified H3.Functions.Traversal as H3
import qualified H3.Functions.Types as H3
import Test.Hspec.Expectations
import Test.Tasty
import Test.Tasty.HUnit

traversalTests :: TestTree
traversalTests =
  testGroup
    "Traversal"
    [ gridDiskTest,
      gridDiskDistancesTest,
      gridDiskUnsafeTest,
      gridDiskDistancesUnsafeTest,
      gridDiskDistancesSafeTest,
      gridDisksUnsafeTest,
      gridRingUnsafeTest,
      gridPathCellsTest,
      gridDistanceTest,
      cellToLocalIjTest,
      localIjToCellTest
    ]

gridDiskTest :: TestTree
gridDiskTest = testCase "gridDisk" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 7
  void $ H3.gridDisk cell 2

gridDiskDistancesTest :: TestTree
gridDiskDistancesTest = testCase "gridDiskDistances" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 7
  void $ H3.gridDiskDistances cell 2

gridDiskUnsafeTest :: TestTree
gridDiskUnsafeTest = testCase "gridDiskUnsafe" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 7
  void $ H3.gridDiskUnsafe cell 2

gridDiskDistancesUnsafeTest :: TestTree
gridDiskDistancesUnsafeTest = testCase "gridDiskDistancesUnsafe" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 7
  void $ H3.gridDiskDistancesUnsafe cell 2

gridDiskDistancesSafeTest :: TestTree
gridDiskDistancesSafeTest = testCase "gridDiskDistancesSafe (Not implemented)" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 7
  H3.gridDiskDistancesSafe cell 2 `shouldThrow` anyException

gridDisksUnsafeTest :: TestTree
gridDisksUnsafeTest = testCase "gridDisksUnsafe" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 7
  cell2 <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 23) (H3.degsToRads (-11))) 7
  void $ H3.gridDisksUnsafe [cell, cell2] 2

gridRingUnsafeTest :: TestTree
gridRingUnsafeTest = testCase "gridRingUnsafe" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 7
  void $ H3.gridRingUnsafe cell 2

gridPathCellsTest :: TestTree
gridPathCellsTest = testCase "gridPathCells" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 7
  cell2 <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 23) (H3.degsToRads (-11))) 7
  void $ H3.gridPathCells cell cell2

gridDistanceTest :: TestTree
gridDistanceTest = testCase "gridDistance" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 7
  cell2 <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 23) (H3.degsToRads (-11))) 7
  void $ H3.gridDistance cell cell2

cellToLocalIjTest :: TestTree
cellToLocalIjTest = testCase "cellToLocalIj" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 7
  cell2 <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 23) (H3.degsToRads (-11))) 7
  void $ H3.cellToLocalIj cell cell2

localIjToCellTest :: TestTree
localIjToCellTest = testCase "localIjToCell" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 7
  cell2 <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 23) (H3.degsToRads (-11))) 7
  tj <- H3.cellToLocalIj cell cell2
  void $ H3.localIjToCell cell tj
