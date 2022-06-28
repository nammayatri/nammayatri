{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Hierarchy (hierarchyTests) where

import Control.Monad
import qualified H3.Functions.Hierarchy as H3
import qualified H3.Functions.Indexing as H3
import qualified H3.Functions.Miscellaneous as H3
import qualified H3.Functions.Types as H3
import Test.Hspec.Expectations
import Test.Tasty
import Test.Tasty.HUnit

hierarchyTests :: TestTree
hierarchyTests =
  testGroup
    "Hierarchy"
    [ cellToParentTest,
      cellToChildrenTest,
      cellToCenterChildTest,
      compactCellsTest,
      uncompactCellsTest
    ]

cellToParentTest :: TestTree
cellToParentTest = testCase "cellToChildren" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 9
  void $ H3.cellToParent cell 8

cellToChildrenTest :: TestTree
cellToChildrenTest = testCase "cellToParent" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 9
  parentCell <- H3.cellToParent cell 8
  void $ H3.cellToChildren parentCell 9

cellToCenterChildTest :: TestTree
cellToCenterChildTest = testCase "cellToCenterChild" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 9
  void $ H3.cellToCenterChild cell 10

compactCellsTest :: TestTree
compactCellsTest = testCase "compactCells" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 9
  parentCell <- H3.cellToParent cell 8
  childCells <- H3.cellToChildren parentCell 9
  void $ H3.compactCells childCells

uncompactCellsTest :: TestTree
uncompactCellsTest = testCase "uncompactCells" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 9
  parentCell <- H3.cellToParent cell 8
  childCells <- H3.cellToChildren parentCell 9
  void $ H3.uncompactCells childCells 10
