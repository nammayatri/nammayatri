{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Functions.DirectedEdges where

import Control.Monad.Catch
import H3.Functions.DirectedEdges.Internal
import H3.Functions.Types
import H3.Functions.Types.Error

areNeighborCells :: (MonadThrow m) => H3Cell -> H3Cell -> m Bool
areNeighborCells origin dest = do
  let (resCode, res) = c_areNeighborCells (fromIntegral origin) (fromIntegral dest)
  throwOnError resCode
  return $ res == 1

cellsToDirectedEdge :: (MonadThrow m) => H3Cell -> H3Cell -> m H3Edge
cellsToDirectedEdge origin dest = do
  let (resCode, edge) = c_cellsToDirectedEdge (fromIntegral origin) (fromIntegral dest)
  throwOnError resCode
  return $ fromIntegral edge

isValidDirectedEdge :: H3Edge -> Bool
isValidDirectedEdge edge = c_isValidDirectedEdge (fromIntegral edge)

getDirectedEdgeOrigin :: (MonadThrow m) => H3Edge -> m H3Cell
getDirectedEdgeOrigin edge = do
  let (resCode, origin) = c_getDirectedEdgeOrigin (fromIntegral edge)
  throwOnError resCode
  return $ fromIntegral origin

getDirectedEdgeDestination :: (MonadThrow m) => H3Edge -> m H3Cell
getDirectedEdgeDestination edge = do
  let (resCode, dest) = c_getDirectedEdgeDestination (fromIntegral edge)
  throwOnError resCode
  return $ fromIntegral dest

directedEdgeToCells :: (MonadThrow m) => H3Edge -> m (H3Cell, H3Cell)
directedEdgeToCells edge = do
  let (resCode, dest) = c_directedEdgeToCells (fromIntegral edge)
  throwOnError resCode
  return (fromIntegral $ head dest, fromIntegral $ Prelude.last dest)

originToDirectedEdges :: (MonadThrow m) => H3Cell -> m [H3Edge]
originToDirectedEdges origin = do
  let (resCode, edges) = c_originToDirectedEdges (fromIntegral origin)
  throwOnError resCode
  return $ fromIntegral <$> edges

directedEdgeToBoundary :: (MonadThrow m) => H3Edge -> m H3CellBoundary
directedEdgeToBoundary origin = do
  let (resCode, boundary) = c_directedEdgeToBoundary (fromIntegral origin)
  throwOnError resCode
  return boundary
