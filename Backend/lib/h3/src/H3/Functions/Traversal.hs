{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Functions.Traversal where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Foreign
import H3.Functions.Traversal.Internal
import H3.Functions.Types
import H3.Functions.Types.Error

gridDisk :: (MonadThrow m, MonadIO m) => H3Cell -> Int -> m [H3Cell]
gridDisk cell dist = do
  size <- maxGridDiskSize dist
  liftIO . allocaArray size $ \arr -> do
    let resCode = c_gridDisk (fromIntegral cell) (fromIntegral dist) arr
    throwOnError resCode
    cells <- peekArray size arr
    return $ fromIntegral <$> cells

maxGridDiskSize :: (MonadThrow m) => Int -> m Int
maxGridDiskSize dist = do
  let (resCode, size) = c_maxGridDiskSize (fromIntegral dist)
  throwOnError resCode
  return $ fromIntegral size

gridDiskDistances :: (MonadThrow m, MonadIO m) => H3Cell -> Int -> m [(H3Cell, Int)]
gridDiskDistances cell dist = do
  size <- maxGridDiskSize dist
  liftIO . allocaArray size $ \cellsArr ->
    allocaArray size $ \distsArr -> do
      let resCode = c_gridDiskDistances (fromIntegral cell) (fromIntegral dist) cellsArr distsArr
      throwOnError resCode
      cells <- peekArray size cellsArr
      dists <- peekArray size distsArr
      return $ zip (fromIntegral <$> cells) (fromIntegral <$> dists)

gridDiskUnsafe :: (MonadThrow m, MonadIO m) => H3Cell -> Int -> m [H3Cell]
gridDiskUnsafe cell dist = do
  size <- maxGridDiskSize dist
  liftIO . allocaArray size $ \arr -> do
    let resCode = c_gridDiskUnsafe (fromIntegral cell) (fromIntegral dist) arr
    throwOnError resCode
    cells <- peekArray size arr
    return $ fromIntegral <$> cells

gridDiskDistancesUnsafe :: (MonadThrow m, MonadIO m) => H3Cell -> Int -> m [(H3Cell, Int)]
gridDiskDistancesUnsafe cell dist = do
  size <- maxGridDiskSize dist
  liftIO . allocaArray size $ \cellsArr ->
    allocaArray size $ \distsArr -> do
      let resCode = c_gridDiskDistancesUnsafe (fromIntegral cell) (fromIntegral dist) cellsArr distsArr
      throwOnError resCode
      cells <- peekArray size cellsArr
      dists <- peekArray size distsArr
      return $ zip (fromIntegral <$> cells) (fromIntegral <$> dists)

-- TODO: This func never ends
gridDiskDistancesSafe :: (MonadThrow m, MonadIO m) => H3Cell -> Int -> m [(H3Cell, Int)]
gridDiskDistancesSafe _cell _dist = do
  error "gridDiskDistancesSafe Not implemented"

-- size <- maxGridDiskSize dist
-- liftIO . allocaArray size $ \cellsArr ->
--   allocaArray size $ \distsArr -> do
--     let resCode = c_gridDiskDistancesSafe (fromIntegral cell) (fromIntegral dist) cellsArr distsArr
--     throwOnError resCode
--     cells <- peekArray size cellsArr
--     dists <- peekArray size distsArr
--     return $ zip (fromIntegral <$> cells) (fromIntegral <$> dists)

gridDisksUnsafe :: (MonadThrow m, MonadIO m) => [H3Cell] -> Int -> m [H3Cell]
gridDisksUnsafe cells dist = do
  size <- maxGridDiskSize dist
  let cellsNum = length cells
  liftIO . allocaArray (size * cellsNum) $ \resCellsArr ->
    allocaArray cellsNum $ \srcCellsArr -> do
      pokeArray srcCellsArr $ fromIntegral <$> cells
      let resCode = c_gridDisksUnsafe srcCellsArr (fromIntegral cellsNum) (fromIntegral dist) resCellsArr
      throwOnError resCode
      resCells <- peekArray size resCellsArr
      return (fromIntegral <$> resCells)

gridRingUnsafe :: (MonadThrow m, MonadIO m) => H3Cell -> Int -> m [H3Cell]
gridRingUnsafe cell dist = do
  size <- maxGridDiskSize dist
  liftIO . allocaArray size $ \arr -> do
    let resCode = c_gridRingUnsafe (fromIntegral cell) (fromIntegral dist) arr
    throwOnError resCode
    cells <- peekArray size arr
    return $ fromIntegral <$> cells

gridPathCells :: (MonadThrow m, MonadIO m) => H3Cell -> H3Cell -> m [H3Cell]
gridPathCells startCell endCell = do
  size <- gridPathCellsSize startCell endCell
  liftIO . allocaArray size $ \arr -> do
    let resCode = c_gridPathCells (fromIntegral startCell) (fromIntegral endCell) arr
    throwOnError resCode
    cells <- peekArray size arr
    return $ fromIntegral <$> cells

gridPathCellsSize :: (MonadThrow m) => H3Cell -> H3Cell -> m Int
gridPathCellsSize startCell endCell = do
  let (resCode, size) = c_gridPathCellsSize (fromIntegral startCell) (fromIntegral endCell)
  throwOnError resCode
  return $ fromIntegral size

gridDistance :: (MonadThrow m) => H3Cell -> H3Cell -> m Int
gridDistance startCell endCell = do
  let (resCode, dist) = c_gridDistance (fromIntegral startCell) (fromIntegral endCell)
  throwOnError resCode
  return $ fromIntegral dist

cellToLocalIj :: (MonadThrow m) => H3Cell -> H3Cell -> m H3CoordIJ
cellToLocalIj originCell h3 = do
  let (resCode, dist) = c_cellToLocalIj (fromIntegral originCell) (fromIntegral h3) 0
  throwOnError resCode
  return dist

localIjToCell :: (MonadThrow m) => H3Cell -> H3CoordIJ -> m H3Cell
localIjToCell originCell ij = do
  let (resCode, dist) = c_localIjToCell (fromIntegral originCell) ij 0
  throwOnError resCode
  return $ fromIntegral dist
