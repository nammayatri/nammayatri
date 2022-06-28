{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Functions.Regions where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Foreign
import H3.Functions.Regions.Internal
import H3.Functions.Types
import H3.Functions.Types.Error

-- TODO: Doesn't work for some reason
polygonToCells :: (MonadThrow m, MonadIO m) => H3GeoPolygon -> H3Resolution -> m [H3Cell]
polygonToCells _geoPolygon _resolution = do
  error "polygonToCells Not implemented"

--   size <- maxPolygonToCellsSize geoPolygon resolution
--   liftIO . allocaArray size $ \arr -> do
--     let resCode = c_polygonToCells geoPolygon (fromIntegral resolution) 0 arr
--     throwOnError resCode
--     cells <- peekArray size arr
--     return $ fromIntegral <$> cells

-- TODO: Always 15???
maxPolygonToCellsSize :: (MonadThrow m) => H3GeoPolygon -> H3Resolution -> m Int
maxPolygonToCellsSize _geoPolygon _resolution = do
  error "maxPolygonToCellsSize Not implemented"

--   let (resCode, size) = c_maxPolygonToCellsSize geoPolygon (fromIntegral resolution) 0
--   throwOnError resCode
--   return $ fromIntegral size

cellsToLinkedMultiPolygon :: (MonadThrow m, MonadIO m) => [H3Cell] -> m H3LinkedGeoPolygon
cellsToLinkedMultiPolygon cellsSet = do
  let size = length cellsSet
  liftIO . allocaArray size $ \arr -> do
    pokeArray arr $ fromIntegral <$> cellsSet
    (resCode, polygon) <- c_cellsToLinkedMultiPolygon arr (fromIntegral size)
    throwOnError resCode
    return polygon

destroyLinkedMultiPolygon :: (MonadThrow m, MonadIO m) => H3LinkedGeoPolygon -> m ()
destroyLinkedMultiPolygon polygon = do
  liftIO $ c_destroyLinkedMultiPolygon polygon
