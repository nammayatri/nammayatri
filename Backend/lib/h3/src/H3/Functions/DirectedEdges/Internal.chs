{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# OPTIONS_GHC -Wno-all #-}
{-# OPTIONS_GHC -Wno-compat #-}
{-# OPTIONS_GHC -Wno-identities #-}
module H3.Functions.DirectedEdges.Internal where

{#import H3.Functions.Types.Internal#}
import Foreign
import Foreign.C.Types

#include <h3/h3api.h>

{#context lib="h3api"#}

{#fun pure areNeighborCells as c_areNeighborCells {`CULong', `CULong', alloca- `CInt' peek*} -> `H3ResultCode' #}

{#fun pure cellsToDirectedEdge as c_cellsToDirectedEdge {`CULong', `CULong', alloca- `CULong' peek*} -> `H3ResultCode' #}

{#fun pure isValidDirectedEdge as c_isValidDirectedEdge {`CULong'} -> `Bool' #}

{#fun pure getDirectedEdgeOrigin as c_getDirectedEdgeOrigin {`CULong', alloca- `CULong' peek*} -> `H3ResultCode' #}

{#fun pure getDirectedEdgeDestination as c_getDirectedEdgeDestination {`CULong', alloca- `CULong' peek*} -> `H3ResultCode' #}

c_originToDirectedEdgesInMarshal1 :: (Ptr CULong -> IO a) -> IO a
c_originToDirectedEdgesInMarshal1 = allocaArray 6
c_originToDirectedEdgesOutMarshal1 :: Ptr CULong -> IO [CULong]
c_originToDirectedEdgesOutMarshal1 = peekArray 6
{#fun pure originToDirectedEdges as c_originToDirectedEdges {`CULong', c_originToDirectedEdgesInMarshal1- `[CULong]' c_originToDirectedEdgesOutMarshal1*} -> `H3ResultCode' #}

c_directedEdgeToCellsInMarshal1 :: (Ptr CULong -> IO a) -> IO a
c_directedEdgeToCellsInMarshal1 = allocaArray 2
c_directedEdgeToCellsOutMarshal1 :: Ptr CULong -> IO [CULong]
c_directedEdgeToCellsOutMarshal1 = peekArray 2
{#fun pure directedEdgeToCells as c_directedEdgeToCells {`CULong', c_directedEdgeToCellsInMarshal1- `[CULong]' c_directedEdgeToCellsOutMarshal1*} -> `H3ResultCode' #}

{#fun pure directedEdgeToBoundary as c_directedEdgeToBoundary {`CULong', alloca- `H3CellBoundary' fromH3CellBoundaryC*} -> `H3ResultCode' #}