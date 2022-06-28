{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Regions (regionsTests) where

import Control.Exception (try)
import Control.Monad
import qualified H3.Functions.DirectedEdges as H3
import qualified H3.Functions.Indexing as H3
import qualified H3.Functions.Miscellaneous as H3
import qualified H3.Functions.Regions as H3
import qualified H3.Functions.Traversal as H3
import qualified H3.Functions.Types as H3
import Test.Hspec.Expectations
import Test.Tasty
import Test.Tasty.HUnit

regionsTests :: TestTree
regionsTests =
  testGroup
    "Regions"
    [ polygonToCellsTest,
      cellsToLinkedMultiPolygonTest
    ]

polygonToCellsTest :: TestTree
polygonToCellsTest = testCase "polygonToCells (Not implemented)" $ do
  let geoloop =
        H3.H3GeoLoop
          3
          [ H3.H3LatLng (H3.degsToRads 37.813318999983238) (H3.degsToRads (-122.4089866999972145)),
            H3.H3LatLng (H3.degsToRads 37.7198061999978478) (H3.degsToRads (-122.3544736999993603)),
            H3.H3LatLng (H3.degsToRads 37.8151571999998453) (H3.degsToRads (-122.4798767000009008))
          ]
      hole =
        H3.H3GeoLoop
          3
          [ H3.H3LatLng (H3.degsToRads 24) (H3.degsToRads (-10)),
            H3.H3LatLng (H3.degsToRads 25) (H3.degsToRads (-9)),
            H3.H3LatLng (H3.degsToRads 24) (H3.degsToRads (-9))
          ]
  let geoPolygon =
        H3.H3GeoPolygon
          { geoloop = geoloop,
            numHoles = 1,
            holes = [hole]
          }
  H3.polygonToCells geoPolygon 7 `shouldThrow` anyException

cellsToLinkedMultiPolygonTest :: TestTree
cellsToLinkedMultiPolygonTest = testCase "cellsToLinkedMultiPolygon && destroyLinkedMultiPolygon" $ do
  cell <- H3.latLngToCell (H3.H3LatLng (H3.degsToRads 22) (H3.degsToRads (-11))) 10
  cells <- H3.gridDisk cell 2
  linkedPolygon <- H3.cellsToLinkedMultiPolygon cells
  H3.destroyLinkedMultiPolygon linkedPolygon
