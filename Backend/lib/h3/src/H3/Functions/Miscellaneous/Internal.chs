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
module H3.Functions.Miscellaneous.Internal where

{#import H3.Functions.Types.Internal#}
import Foreign
import Foreign.C.Types

#include <h3/h3api.h>

{#context lib="h3api"#}

{#fun pure degsToRads as c_degsToRads {`CDouble'} -> `CDouble'#}

{#fun pure radsToDegs as c_radsToDegs {`CDouble'} -> `CDouble'#}

{#fun pure getHexagonAreaAvgKm2 as c_getHexagonAreaAvgKm2 {`CInt', alloca- `CDouble' peek*} -> `H3ResultCode'#}

{#fun pure getHexagonAreaAvgM2 as c_getHexagonAreaAvgM2 {`CInt', alloca- `CDouble' peek*} -> `H3ResultCode'#}

{#fun pure cellAreaRads2 as c_cellAreaRads2 {`CInt', alloca- `CDouble' peek*} -> `H3ResultCode' #}

{#fun pure cellAreaKm2 as c_cellAreaKm2 {`CInt', alloca- `CDouble' peek*} -> `H3ResultCode' #}

{#fun pure cellAreaM2 as c_cellAreaM2 {`CInt', alloca- `CDouble' peek*} -> `H3ResultCode' #}

{#fun pure getHexagonEdgeLengthAvgKm as c_getHexagonEdgeLengthAvgKm {`CInt', alloca- `CDouble' peek*} -> `H3ResultCode'#}

{#fun pure getHexagonEdgeLengthAvgM as c_getHexagonEdgeLengthAvgM {`CInt', alloca- `CDouble' peek*} -> `H3ResultCode'#}

{#fun pure exactEdgeLengthKm as c_exactEdgeLengthKm {`CULong', alloca- `CDouble' peek*} -> `H3ResultCode' #}

{#fun pure exactEdgeLengthM as c_exactEdgeLengthM {`CULong', alloca- `CDouble' peek*} -> `H3ResultCode' #}

{#fun pure exactEdgeLengthRads as c_exactEdgeLengthRads {`CULong', alloca- `CDouble' peek*} -> `H3ResultCode' #}

{#fun pure getNumCells as c_getNumCells {`CInt', alloca- `CLong' peek*} -> `H3ResultCode' #}

{#fun pure res0CellCount as c_res0CellCount {} -> `CInt' #}

c_getRes0CellsInMarshal :: (Ptr CULong -> IO a) -> IO a
c_getRes0CellsInMarshal = allocaArray (fromIntegral c_res0CellCount)
c_getRes0CellsOutMarshal :: Ptr CULong -> IO [CULong]
c_getRes0CellsOutMarshal = peekArray (fromIntegral c_res0CellCount)
{#fun pure getRes0Cells as c_getRes0Cells {c_getRes0CellsInMarshal- `[CULong]' c_getRes0CellsOutMarshal*} -> `H3ResultCode' #}

{#fun pure pentagonCount as c_pentagonCount {} -> `CInt' #}

c_getPentagonsInMarshal :: (Ptr CULong -> IO a) -> IO a
c_getPentagonsInMarshal = allocaArray (fromIntegral c_pentagonCount)
c_getPentagonsOutMarshal :: Ptr CULong -> IO [CULong]
c_getPentagonsOutMarshal = peekArray (fromIntegral c_pentagonCount)
{#fun pure getPentagons as c_getPentagons {`CInt', c_getPentagonsInMarshal- `[CULong]' c_getPentagonsOutMarshal*} -> `H3ResultCode' #}

{#fun pure distanceKm as c_distanceKm {with* `H3LatLng', with* `H3LatLng'} -> `CDouble' #}

{#fun pure distanceM as c_distanceM {with* `H3LatLng', with* `H3LatLng'} -> `CDouble' #}

{#fun pure distanceRads as c_distanceRads {with* `H3LatLng', with* `H3LatLng'} -> `CDouble' #}