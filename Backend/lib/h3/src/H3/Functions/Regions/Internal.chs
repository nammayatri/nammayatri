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
module H3.Functions.Regions.Internal where

{#import H3.Functions.Types.Internal#}
import Foreign
import Foreign.C.Types

#include <h3/h3api.h>

{#context lib="h3api"#}

{#fun pure polygonToCells as c_polygonToCells {toH3GeoPolygonCPtr* `H3GeoPolygon', `CInt', `CUInt', id `Ptr CULong'} -> `H3ResultCode' #}

{#fun pure maxPolygonToCellsSize as c_maxPolygonToCellsSize {toH3GeoPolygonCPtr* `H3GeoPolygon', `CInt', `CUInt', alloca- `CLong' peek*} -> `H3ResultCode' #}

{#fun unsafe cellsToLinkedMultiPolygon as c_cellsToLinkedMultiPolygon {id `Ptr CULong', `CInt', alloca- `H3LinkedGeoPolygon' fromH3LinkedGeoPolygonCPtr*} -> `H3ResultCode' #}

{#fun unsafe destroyLinkedMultiPolygon as c_destroyLinkedMultiPolygon {toH3LinkedGeoPolygonCPtr* `H3LinkedGeoPolygon'} -> `()' #}
