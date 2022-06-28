{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Functions.Hierarchy where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Foreign
import H3.Functions.Hierarchy.Internal
import H3.Functions.Types
import H3.Functions.Types.Error

cellToParent :: (MonadThrow m) => H3Cell -> H3Resolution -> m H3Cell
cellToParent originCell parentRes = do
  let (resCode, parentCell) = c_cellToParent (fromIntegral originCell) (fromIntegral parentRes)
  throwOnError resCode
  return $ fromIntegral parentCell

cellToChildren :: (MonadThrow m, MonadIO m) => H3Cell -> H3Resolution -> m [H3Cell]
cellToChildren startCell childRes = do
  size <- cellToChildrenSize startCell childRes
  liftIO . allocaArray size $ \arr -> do
    let resCode = c_cellToChildren (fromIntegral startCell) (fromIntegral childRes) arr
    throwOnError resCode
    cells <- peekArray size arr
    return $ fromIntegral <$> cells

cellToChildrenSize :: (MonadThrow m) => H3Cell -> H3Resolution -> m Int
cellToChildrenSize originCell childRes = do
  let (resCode, size) = c_cellToChildrenSize (fromIntegral originCell) (fromIntegral childRes)
  throwOnError resCode
  return $ fromIntegral size

cellToCenterChild :: (MonadThrow m) => H3Cell -> H3Resolution -> m H3Cell
cellToCenterChild startCell childRes = do
  let (resCode, centerChild) = c_cellToCenterChild (fromIntegral startCell) (fromIntegral childRes)
  throwOnError resCode
  return $ fromIntegral centerChild

compactCells :: (MonadThrow m, MonadIO m) => [H3Cell] -> m [H3Cell]
compactCells cellSet = do
  let size = length cellSet
  liftIO . allocaArray size $ \cellSetArr ->
    allocaArray size $ \compactedSetArr -> do
      pokeArray cellSetArr $ fromIntegral <$> cellSet
      let resCode = c_compactCells cellSetArr compactedSetArr (fromIntegral size)
      throwOnError resCode
      compactedSet <- peekArray size compactedSetArr
      return $ fromIntegral <$> compactedSet

uncompactCells :: (MonadThrow m, MonadIO m) => [H3Cell] -> H3Resolution -> m [H3Cell]
uncompactCells compactedSet res = do
  let compactedSize = length compactedSet
  uncompactSize <- uncompactCellsSize compactedSet res
  liftIO . allocaArray uncompactSize $ \uncompactedSetArr ->
    allocaArray compactedSize $ \compactedSetArr -> do
      pokeArray compactedSetArr $ fromIntegral <$> compactedSet
      let resCode = c_uncompactCells compactedSetArr (fromIntegral compactedSize) uncompactedSetArr (fromIntegral uncompactSize) (fromIntegral res)
      throwOnError resCode
      uncompactedSet <- peekArray uncompactSize uncompactedSetArr
      return $ fromIntegral <$> uncompactedSet

uncompactCellsSize :: (MonadThrow m, MonadIO m) => [H3Cell] -> H3Resolution -> m Int
uncompactCellsSize compactedSet res = do
  let compactedSetSize = length compactedSet
  liftIO . allocaArray compactedSetSize $ \compactedSetArr -> do
    pokeArray compactedSetArr $ fromIntegral <$> compactedSet
    let (resCode, size) = c_uncompactCellsSize compactedSetArr (fromIntegral compactedSetSize) (fromIntegral res)
    throwOnError resCode
    return $ fromIntegral size
