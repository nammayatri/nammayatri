{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Functions.Types
  ( module H3.Functions.Types,
    module Reexport,
  )
where

import Foreign
import H3.Functions.Types.Error as Reexport (H3Error (..))
import H3.Functions.Types.Internal
import H3.Functions.Types.Internal as Reexport
  ( H3Cell (..),
    H3CellBoundary (..),
    H3CoordIJ (..),
    H3Edge (..),
    H3GeoLoop (..),
    H3GeoPolygon (..),
    H3LatLng (..),
    H3LinkedGeoLoop (..),
    H3LinkedGeoPolygon (..),
    H3LinkedLatLng (..),
    H3Vertex (..),
    Radians,
  )

type H3Resolution = Int

defaultResolution :: H3Resolution
defaultResolution = 10

type Degrees = Double

type Radians2 = Double

type Degrees2 = Double

type Kilometers = Double

type Meters = Double

type Kilometers2 = Double

type Meters2 = Double

type H3Index = Word64
