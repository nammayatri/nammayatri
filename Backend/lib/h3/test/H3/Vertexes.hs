{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Vertexes (vertexesTests) where

import Control.Monad
import qualified H3.Functions.Indexing as H3
import qualified H3.Functions.Miscellaneous as H3
import qualified H3.Functions.Types as H3
import qualified H3.Functions.Vertexes as H3
import Test.Hspec.Expectations
import Test.Tasty
import Test.Tasty.HUnit

vertexesTests :: TestTree
vertexesTests =
  testGroup
    "Vertexes"
    [ cellToVertexTest,
      cellToVertexesTest,
      isValidVertexTest,
      vertexToLatLngTest
    ]

cellToVertexTest :: TestTree
cellToVertexTest = testCase "cellToVertex" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  void $ H3.cellToVertex cell 1

cellToVertexesTest :: TestTree
cellToVertexesTest = testCase "cellToVertexes" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  vertex <- H3.cellToVertex cell 1
  void $ H3.cellToVertexes cell

isValidVertexTest :: TestTree
isValidVertexTest = testCase "isValidVertex" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  vertex <- H3.cellToVertex cell 1
  H3.isValidVertex vertex `shouldBe` True

vertexToLatLngTest :: TestTree
vertexToLatLngTest = testCase "vertexToLatLng" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  vertex <- H3.cellToVertex cell 1
  void $ H3.vertexToLatLng vertex
