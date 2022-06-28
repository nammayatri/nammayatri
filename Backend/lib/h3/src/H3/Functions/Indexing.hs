{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Functions.Indexing where

import Control.Monad.Catch
import H3.Functions.Indexing.Internal
import H3.Functions.Types
import H3.Functions.Types.Error

latLngToCell :: (MonadThrow m) => H3LatLng -> H3Resolution -> m H3Cell
latLngToCell latLng resolution = do
  let (resCode, cell) = c_latLngToCell latLng (fromIntegral resolution)
  throwOnError resCode
  return $ fromIntegral cell

cellToLatLng :: (MonadThrow m) => H3Cell -> m H3LatLng
cellToLatLng cell = do
  let (resCode, latLng) = c_cellToLatLng (fromIntegral cell)
  throwOnError resCode
  return latLng

cellToBoundary :: (MonadThrow m) => H3Cell -> m H3CellBoundary
cellToBoundary cell = do
  let (resCode, cellBoundaty) = c_cellToBoundary (fromIntegral cell)
  throwOnError resCode
  return cellBoundaty
