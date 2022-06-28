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
module H3.Functions.Hierarchy.Internal where

{#import H3.Functions.Types.Internal#}
import Foreign
import Foreign.C.Types

#include <h3/h3api.h>

{#context lib="h3api"#}

{#fun pure cellToParent as c_cellToParent {`CULong', `CInt', alloca- `CULong' peek*} -> `H3ResultCode' #}

{#fun pure cellToChildren as c_cellToChildren {`CULong', `CInt', id `Ptr CULong'} -> `H3ResultCode' #}

{#fun pure cellToChildrenSize as c_cellToChildrenSize {`CULong', `CInt', alloca- `CLong' peek*} -> `H3ResultCode' #}

{#fun pure cellToCenterChild as c_cellToCenterChild {`CULong', `CInt', alloca- `CULong' peek*} -> `H3ResultCode' #}

{#fun pure compactCells as c_compactCells {id `Ptr CULong', id `Ptr CULong', `CULong'} -> `H3ResultCode' #}

{#fun pure uncompactCells as c_uncompactCells {id `Ptr CULong', `CULong', id `Ptr CULong', `CULong', `CInt'} -> `H3ResultCode' #}

{#fun pure uncompactCellsSize as c_uncompactCellsSize {id `Ptr CULong', `CLong', `CInt', alloca- `CLong' peek*} -> `H3ResultCode' #}