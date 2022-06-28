{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import H3.DirectedEdges
import H3.Hierarchy
import H3.Indexing
import H3.Inspection
import H3.Miscellaneous
import H3.Regions
import H3.Traversal
import H3.Vertexes
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain spec

--Simple tests to check that we get some response from C side.
spec :: TestTree
spec =
  testGroup
    "H3"
    [ directedEdgesTests,
      hierarchyTests,
      indexingTests,
      inspectionTests,
      miscellaneousTests,
      regionsTests,
      traversalTests,
      vertexesTests
    ]
