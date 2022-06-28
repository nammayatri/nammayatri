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
module H3.Functions.Inspection.Internal where

{#import H3.Functions.Types.Internal#}
import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <h3/h3api.h>

{#context lib="h3api"#}

{#fun pure getResolution as c_getResolution {`CULong'} -> `CInt' #}

{#fun pure getBaseCellNumber as c_getBaseCellNumber {`CULong'} -> `CInt' #}

{#fun pure stringToH3 as c_stringToH3 {`String', alloca- `CULong' peek*} -> `H3ResultCode' #}

c_h3ToString :: CULong -> (H3ResultCode, String)
c_h3ToString cell = c_h3ToString' cell (fromIntegral 17)

c_h3ToStringInMarshal1 :: (CString -> IO a) -> IO a
c_h3ToStringInMarshal1 = allocaArray 17
{#fun pure h3ToString as c_h3ToString' {`CULong', c_h3ToStringInMarshal1- `String' peekCString*, fromIntegral `CSize' } -> `H3ResultCode' #}

{#fun pure isValidCell as c_isValidCell {`CULong'} -> `CInt' #}

{#fun pure isResClassIII as c_isResClassIII {`CULong'} -> `CInt' #}

{#fun pure isPentagon as c_isPentagon {`CULong'} -> `CInt' #}

{#fun pure getIcosahedronFaces as c_getIcosahedronFaces {`CULong', id `Ptr CInt' } -> `H3ResultCode' #}

{#fun pure maxFaceCount as c_maxFaceCount {`CULong', alloca- `CInt' peek*} -> `H3ResultCode' #}