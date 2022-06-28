{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.DirectedEdges (directedEdgesTests) where

import Control.Monad
import qualified H3.Functions.DirectedEdges as H3
import qualified H3.Functions.Indexing as H3
import qualified H3.Functions.Miscellaneous as H3
import qualified H3.Functions.Types as H3
import Test.Hspec.Expectations
import Test.Tasty
import Test.Tasty.HUnit

directedEdgesTests :: TestTree
directedEdgesTests =
  testGroup
    "DirectedEdges"
    [ originToDirectedEdgesTest,
      getDirectedEdgeOriginTest,
      getDirectedEdgeDestinationTest,
      areNeighborCellsTest,
      isValidDirectedEdgeTest,
      cellsToDirectedEdgeTest,
      directedEdgeToCellsTest,
      directedEdgeToBoundaryTest
    ]

originToDirectedEdgesTest :: TestTree
originToDirectedEdgesTest = testCase "originToDirectedEdges" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  void $ H3.originToDirectedEdges cell

getDirectedEdgeOriginTest :: TestTree
getDirectedEdgeOriginTest = testCase "getDirectedEdgeOrigin" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  edges <- H3.originToDirectedEdges cell
  void $ H3.getDirectedEdgeOrigin $ head edges

getDirectedEdgeDestinationTest :: TestTree
getDirectedEdgeDestinationTest = testCase "getDirectedEdgeDestination" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  edges <- H3.originToDirectedEdges cell
  void $ H3.getDirectedEdgeDestination $ head edges

areNeighborCellsTest :: TestTree
areNeighborCellsTest = testCase "areNeighborCells" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  edges <- H3.originToDirectedEdges cell
  origin <- H3.getDirectedEdgeOrigin $ head edges
  dest <- H3.getDirectedEdgeDestination $ head edges
  res <- H3.areNeighborCells origin dest
  res `shouldBe` True

isValidDirectedEdgeTest :: TestTree
isValidDirectedEdgeTest = testCase "isValidDirectedEdge" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  edges <- H3.originToDirectedEdges cell
  let res = H3.isValidDirectedEdge $ head edges
  res `shouldBe` True

cellsToDirectedEdgeTest :: TestTree
cellsToDirectedEdgeTest = testCase "cellsToDirectedEdge" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  edges <- H3.originToDirectedEdges cell
  origin <- H3.getDirectedEdgeOrigin $ head edges
  dest <- H3.getDirectedEdgeDestination $ head edges
  edge <- H3.cellsToDirectedEdge origin dest
  edge `shouldBe` head edges

directedEdgeToCellsTest :: TestTree
directedEdgeToCellsTest = testCase "directedEdgeToCells" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  edges <- H3.originToDirectedEdges cell
  origin <- H3.getDirectedEdgeOrigin $ head edges
  dest <- H3.getDirectedEdgeDestination $ head edges
  edge <- H3.cellsToDirectedEdge origin dest
  void $ H3.directedEdgeToCells edge

directedEdgeToBoundaryTest :: TestTree
directedEdgeToBoundaryTest = testCase "directedEdgeToBoundary" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  edges <- H3.originToDirectedEdges cell
  origin <- H3.getDirectedEdgeOrigin $ head edges
  dest <- H3.getDirectedEdgeDestination $ head edges
  edge <- H3.cellsToDirectedEdge origin dest
  void $ H3.directedEdgeToBoundary edge
