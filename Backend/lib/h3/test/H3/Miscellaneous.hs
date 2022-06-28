{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Miscellaneous (miscellaneousTests) where

import Control.Monad
import qualified H3.Functions.DirectedEdges as H3
import qualified H3.Functions.Indexing as H3
import qualified H3.Functions.Miscellaneous as H3
import qualified H3.Functions.Types as H3
import Test.Hspec.Expectations
import Test.Tasty
import Test.Tasty.HUnit

miscellaneousTests :: TestTree
miscellaneousTests =
  testGroup
    "Miscellaneous"
    [ radsToDegsTest,
      degsToRadsTest,
      getHexagonAreaAvgKm2Test,
      getHexagonAreaAvgM2Test,
      cellAreaKm2Test,
      cellAreaM2Test,
      getHexagonEdgeLengthAvgKmTest,
      getHexagonEdgeLengthAvgMTest,
      exactEdgeLengthKmTest,
      exactEdgeLengthMTest,
      exactEdgeLengthRadsTest,
      getNumCellsTest,
      getRes0CellsTest,
      getPentagonsTest,
      distanceKmTest,
      distanceMTest,
      distanceRadsTest
    ]

radsToDegsTest :: TestTree
radsToDegsTest = testCase "radsToDegs" $ do
  let degs = H3.radsToDegs 10
  return $ degs `seq` ()

degsToRadsTest :: TestTree
degsToRadsTest = testCase "degsToRads" $ do
  let degs = H3.radsToDegs 10
      rads = H3.degsToRads degs
  rads `shouldBe` 10

getHexagonAreaAvgKm2Test :: TestTree
getHexagonAreaAvgKm2Test = testCase "getHexagonAreaAvgKm2" $ do
  void $ H3.getHexagonAreaAvgKm2 10

getHexagonAreaAvgM2Test :: TestTree
getHexagonAreaAvgM2Test = testCase "getHexagonAreaAvgM2" $ do
  void $ H3.getHexagonAreaAvgM2 10

cellAreaKm2Test :: TestTree
cellAreaKm2Test = testCase "cellAreaKm2" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  void $ H3.cellAreaKm2 cell

cellAreaM2Test :: TestTree
cellAreaM2Test = testCase "cellAreaM2" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  void $ H3.cellAreaM2 cell

getHexagonEdgeLengthAvgKmTest :: TestTree
getHexagonEdgeLengthAvgKmTest = testCase "getHexagonEdgeLengthAvgKm" $ do
  void $ H3.getHexagonEdgeLengthAvgKm 10

getHexagonEdgeLengthAvgMTest :: TestTree
getHexagonEdgeLengthAvgMTest = testCase "getHexagonEdgeLengthAvgM" $ do
  void $ H3.getHexagonEdgeLengthAvgM 10

exactEdgeLengthKmTest :: TestTree
exactEdgeLengthKmTest = testCase "exactEdgeLengthKm" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  edges <- H3.originToDirectedEdges cell
  (origin, dest) <- H3.directedEdgeToCells $ head edges
  edge <- H3.cellsToDirectedEdge origin dest
  H3.isValidDirectedEdge edge `shouldBe` True
  void $ H3.exactEdgeLengthKm edge

exactEdgeLengthMTest :: TestTree
exactEdgeLengthMTest = testCase "exactEdgeLengthM" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  edges <- H3.originToDirectedEdges cell
  (origin, dest) <- H3.directedEdgeToCells $ head edges
  edge <- H3.cellsToDirectedEdge origin dest
  H3.isValidDirectedEdge edge `shouldBe` True
  void $ H3.exactEdgeLengthM edge

exactEdgeLengthRadsTest :: TestTree
exactEdgeLengthRadsTest = testCase "exactEdgeLengthRads" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  cell <- H3.latLngToCell latLong 10
  edges <- H3.originToDirectedEdges cell
  (origin, dest) <- H3.directedEdgeToCells $ head edges
  edge <- H3.cellsToDirectedEdge origin dest
  H3.isValidDirectedEdge edge `shouldBe` True
  void $ H3.exactEdgeLengthRads edge

getNumCellsTest :: TestTree
getNumCellsTest = testCase "getNumCells" $ do
  void $ H3.getNumCells 10

getRes0CellsTest :: TestTree
getRes0CellsTest = testCase "getRes0Cells" $ do
  void H3.getRes0Cells

getPentagonsTest :: TestTree
getPentagonsTest = testCase "getPentagons" $ do
  void $ H3.getPentagons 10

distanceKmTest :: TestTree
distanceKmTest = testCase "distanceKm" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  let latLong1 = H3.H3LatLng (H3.degsToRads 44) (H3.degsToRads (-21))
  cell <- H3.latLngToCell latLong 10
  cell1 <- H3.latLngToCell latLong1 10
  let dist = H3.distanceKm latLong latLong1
  return $ dist `seq` ()

distanceMTest :: TestTree
distanceMTest = testCase "distanceM" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  let latLong1 = H3.H3LatLng (H3.degsToRads 44) (H3.degsToRads (-21))
  cell <- H3.latLngToCell latLong 10
  cell1 <- H3.latLngToCell latLong1 10
  let dist = H3.distanceM latLong latLong1
  return $ dist `seq` ()

distanceRadsTest :: TestTree
distanceRadsTest = testCase "distanceRads" $ do
  let latLong = H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))
  let latLong1 = H3.H3LatLng (H3.degsToRads 44) (H3.degsToRads (-21))
  cell <- H3.latLngToCell latLong 10
  cell1 <- H3.latLngToCell latLong1 10
  let dist = H3.distanceRads latLong latLong1
  return $ dist `seq` ()
