{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Functions.Miscellaneous where

import Control.Monad.Catch
import H3.Functions.Miscellaneous.Internal
import H3.Functions.Types
import H3.Functions.Types.Error (throwOnError)

degsToRads :: Degrees -> Radians
degsToRads = realToFrac . c_degsToRads . realToFrac

radsToDegs :: Radians -> Degrees
radsToDegs = realToFrac . c_radsToDegs . realToFrac

getHexagonAreaAvgKm2 :: (MonadThrow m) => H3Resolution -> m Kilometers2
getHexagonAreaAvgKm2 resol = do
  let (resCode, area) = c_getHexagonAreaAvgKm2 (fromIntegral resol)
  throwOnError resCode
  return $ realToFrac area

getHexagonAreaAvgM2 :: (MonadThrow m) => H3Resolution -> m Meters2
getHexagonAreaAvgM2 resol = do
  let (resCode, area) = c_getHexagonAreaAvgM2 (fromIntegral resol)
  throwOnError resCode
  return $ realToFrac area

cellAreaKm2 :: (MonadThrow m) => H3Cell -> m Kilometers2
cellAreaKm2 cell = do
  let (resCode, km2) = c_cellAreaKm2 (fromIntegral cell)
  throwOnError resCode
  return $ realToFrac km2

cellAreaM2 :: (MonadThrow m) => H3Cell -> m Meters2
cellAreaM2 cell = do
  let (resCode, m2) = c_cellAreaM2 (fromIntegral cell)
  throwOnError resCode
  return $ realToFrac m2

getHexagonEdgeLengthAvgKm :: (MonadThrow m) => H3Resolution -> m Kilometers
getHexagonEdgeLengthAvgKm resol = do
  let (resCode, edgeLength) = c_getHexagonEdgeLengthAvgKm (fromIntegral resol)
  throwOnError resCode
  return $ realToFrac edgeLength

getHexagonEdgeLengthAvgM :: (MonadThrow m) => H3Resolution -> m Meters
getHexagonEdgeLengthAvgM resol = do
  let (resCode, edgeLength) = c_getHexagonEdgeLengthAvgM (fromIntegral resol)
  throwOnError resCode
  return $ realToFrac edgeLength

exactEdgeLengthKm :: (MonadThrow m) => H3Edge -> m Kilometers
exactEdgeLengthKm edge = do
  let (resCode, km) = c_exactEdgeLengthKm (fromIntegral edge)
  throwOnError resCode
  return $ realToFrac km

exactEdgeLengthM :: (MonadThrow m) => H3Edge -> m Meters
exactEdgeLengthM edge = do
  let (resCode, m) = c_exactEdgeLengthM (fromIntegral edge)
  throwOnError resCode
  return $ realToFrac m

exactEdgeLengthRads :: (MonadThrow m) => H3Edge -> m Radians
exactEdgeLengthRads edge = do
  let (resCode, rads) = c_exactEdgeLengthRads (fromIntegral edge)
  throwOnError resCode
  return $ realToFrac rads

getNumCells :: (MonadThrow m) => H3Resolution -> m Int
getNumCells cell = do
  let (resCode, cellsNum) = c_getNumCells (fromIntegral cell)
  throwOnError resCode
  return $ fromIntegral cellsNum

res0CellCount :: Int
res0CellCount = fromIntegral c_res0CellCount

getRes0Cells :: (MonadThrow m) => m [H3Cell]
getRes0Cells = do
  let (resCode, cells) = c_getRes0Cells
  throwOnError resCode
  return $ fromIntegral <$> cells

pentagonCount :: Int
pentagonCount = fromIntegral c_pentagonCount

getPentagons :: (MonadThrow m) => H3Resolution -> m [H3Cell]
getPentagons resolution = do
  let (resCode, cells) = c_getPentagons $ fromIntegral resolution
  throwOnError resCode
  return $ fromIntegral <$> cells

distanceKm :: H3LatLng -> H3LatLng -> Kilometers
distanceKm latLng = realToFrac . c_distanceKm latLng

distanceM :: H3LatLng -> H3LatLng -> Meters
distanceM latLng = realToFrac . c_distanceM latLng

distanceRads :: H3LatLng -> H3LatLng -> Radians
distanceRads latLng = realToFrac . c_distanceRads latLng
