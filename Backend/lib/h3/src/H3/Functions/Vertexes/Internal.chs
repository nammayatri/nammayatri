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
module H3.Functions.Vertexes.Internal where

{#import H3.Functions.Types.Internal#}
import Foreign
import Foreign.C.Types

#include <h3/h3api.h>

{#context lib="h3api"#}

{#fun pure cellToVertex as c_cellToVertex {`CULong', `CInt', alloca- `CULong' peek*} -> `H3ResultCode' #}

c_cellToVertexesInMarshal1 :: (Ptr CULong -> IO a) -> IO a
c_cellToVertexesInMarshal1 = allocaArray 6
c_cellToVertexesOutMarshal1 :: Ptr CULong -> IO [CULong]
c_cellToVertexesOutMarshal1 = peekArray 6
{#fun pure cellToVertexes as c_cellToVertexes {`CULong', c_cellToVertexesInMarshal1- `[CULong]' c_cellToVertexesOutMarshal1*} -> `H3ResultCode' #}

{#fun pure vertexToLatLng as c_vertexToLatLng {`CULong', alloca- `H3LatLng' peek*} -> `H3ResultCode' #}

{#fun pure isValidVertex as c_isValidVertex {`CULong'} -> `Bool' #}