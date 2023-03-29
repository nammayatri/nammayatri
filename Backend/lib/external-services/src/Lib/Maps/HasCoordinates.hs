{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Maps.HasCoordinates where

-- import qualified Kernel.Types.CommonImport as Types
import Kernel.Prelude
import Kernel.Types.CommonImport

class HasCoordinates a where
  getCoordinates :: a -> LatLong
  default getCoordinates :: (HasField "lat" a Double, HasField "lon" a Double) => a -> LatLong
  getCoordinates = getCoordinatessDefault

getCoordinatessDefault :: (HasField "lat" a Double, HasField "lon" a Double) => a -> LatLong
getCoordinatessDefault loc = LatLong loc.lat loc.lon

instance HasCoordinates LatLong where
  getCoordinates = identity
