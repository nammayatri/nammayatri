{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Functions.Vertexes where

import Control.Monad.Catch
import H3.Functions.Types
import H3.Functions.Types.Error
import H3.Functions.Vertexes.Internal

cellToVertex :: (MonadThrow m) => H3Cell -> Int -> m H3Vertex
cellToVertex cell vertexNum = do
  let (resCode, vertex) = c_cellToVertex (fromIntegral cell) (fromIntegral vertexNum)
  throwOnError resCode
  return $ fromIntegral vertex

cellToVertexes :: (MonadThrow m) => H3Cell -> m [H3Vertex]
cellToVertexes cell = do
  let (resCode, vertexes) = c_cellToVertexes (fromIntegral cell)
  throwOnError resCode
  return $ fromIntegral <$> vertexes

vertexToLatLng :: (MonadThrow m) => H3Vertex -> m H3LatLng
vertexToLatLng cell = do
  let (resCode, latLng) = c_vertexToLatLng (fromIntegral cell)
  throwOnError resCode
  return latLng

isValidVertex :: H3Vertex -> Bool
isValidVertex cell = c_isValidVertex (fromIntegral cell)
