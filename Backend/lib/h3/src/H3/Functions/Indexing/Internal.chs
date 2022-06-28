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
module H3.Functions.Indexing.Internal where

{#import H3.Functions.Types.Internal#}
import Foreign
import Foreign.C.Types

#include <h3/h3api.h>

{#context lib="h3api"#}

{#fun pure latLngToCell as c_latLngToCell {with* `H3LatLng', `CInt', alloca- `CULong' peek*} -> `H3ResultCode' #}

{#fun pure cellToLatLng as c_cellToLatLng {`CULong', alloca- `H3LatLng' peek*} -> `H3ResultCode' #}

{#fun pure cellToBoundary as c_cellToBoundary {`CULong', alloca- `H3CellBoundary' fromH3CellBoundaryC*} -> `H3ResultCode' #}
