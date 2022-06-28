{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Functions.Inspection where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Foreign (allocaArray, peekArray)
import H3.Functions.Inspection.Internal
import H3.Functions.Types
import H3.Functions.Types.Error (throwOnError)

getResolution :: H3Cell -> H3Resolution
getResolution = fromIntegral . c_getResolution . fromIntegral

getBaseCellNumber :: H3Cell -> Int
getBaseCellNumber = fromIntegral . c_getBaseCellNumber . fromIntegral

stringToH3 :: (MonadThrow m) => String -> m H3Index
stringToH3 str = do
  let (resCode, cell) = c_stringToH3 str
  throwOnError resCode
  return $ fromIntegral cell

h3ToString :: (MonadThrow m) => H3Index -> m String
h3ToString cell = do
  let (resCode, str) = c_h3ToString (fromIntegral cell)
  throwOnError resCode
  return str

isValidCell :: H3Cell -> Bool
isValidCell cell = case c_isValidCell $ fromIntegral cell of
  0 -> False
  _ -> True

isResClassIII :: H3Cell -> Bool
isResClassIII cell = case c_isResClassIII $ fromIntegral cell of
  0 -> False
  _ -> True

isPentagon :: H3Cell -> Bool
isPentagon cell = case c_isPentagon $ fromIntegral cell of
  0 -> False
  _ -> True

getIcosahedronFaces :: (MonadThrow m, MonadIO m) => H3Cell -> m [Int]
getIcosahedronFaces cell = do
  size <- maxFaceCount cell
  liftIO . allocaArray size $ \arr -> do
    let resCode = c_getIcosahedronFaces (fromIntegral cell) arr
    throwOnError resCode
    res <- peekArray size arr
    return $ fromIntegral <$> res

maxFaceCount :: (MonadThrow m) => H3Cell -> m Int
maxFaceCount cell = do
  let (resCode, res) = c_maxFaceCount (fromIntegral cell)
  throwOnError resCode
  return $ fromIntegral res
