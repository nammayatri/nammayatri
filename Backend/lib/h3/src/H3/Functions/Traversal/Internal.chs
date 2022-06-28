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
module H3.Functions.Traversal.Internal where

{#import H3.Functions.Types.Internal#}
import Foreign
import Foreign.C.Types

#include <h3/h3api.h>

{#context lib="h3api"#}

{#fun pure gridDisk as c_gridDisk {`CULong', `CInt', id `Ptr CULong'} -> `H3ResultCode' #}

{#fun pure maxGridDiskSize as c_maxGridDiskSize {`CInt', alloca- `CLong' peek*} -> `H3ResultCode' #}

{#fun pure gridDiskDistances as c_gridDiskDistances {`CULong', `CInt', id `Ptr CULong', id `Ptr CInt'} -> `H3ResultCode' #}

{#fun pure gridDiskUnsafe as c_gridDiskUnsafe {`CULong', `CInt', id `Ptr CULong'} -> `H3ResultCode' #}

{#fun pure gridDiskDistancesUnsafe as c_gridDiskDistancesUnsafe {`CULong', `CInt', id `Ptr CULong', id `Ptr CInt'} -> `H3ResultCode' #}

{#fun pure gridDiskDistancesSafe as c_gridDiskDistancesSafe {`CULong', `CInt', id `Ptr CULong', id `Ptr CInt'} -> `H3ResultCode' #}

{#fun pure gridDisksUnsafe as c_gridDisksUnsafe {id `Ptr CULong', `CInt', `CInt', id `Ptr CULong'} -> `H3ResultCode' #}

{#fun pure gridRingUnsafe as c_gridRingUnsafe {`CULong', `CInt', id `Ptr CULong'} -> `H3ResultCode' #}

{#fun pure gridPathCells as c_gridPathCells {`CULong', `CULong', id `Ptr CULong'} -> `H3ResultCode' #}

{#fun pure gridPathCellsSize as c_gridPathCellsSize {`CULong', `CULong', alloca- `CLong' peek* } -> `H3ResultCode' #}

{#fun pure gridDistance as c_gridDistance {`CULong', `CULong', alloca- `CLong' peek* } -> `H3ResultCode' #}

{#fun pure cellToLocalIj as c_cellToLocalIj {`CULong', `CULong', `CUInt', alloca- `H3CoordIJ' peek* } -> `H3ResultCode' #}

{#fun pure localIjToCell as c_localIjToCell {`CULong', with* `H3CoordIJ', `CUInt', alloca- `CULong' peek* } -> `H3ResultCode' #}

