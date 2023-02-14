 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module RouteExtractor where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Kernel.External.Maps.Types (LatLong (LatLong))
import Kernel.Prelude
import RenderTrack
import System.Environment
import qualified Text.XML as XML

deriving instance Read LatLong

-- we don't want to ever use Read instance in production code,
-- so let this instance be here

main :: IO ()
main = do
  s <- getContents
  typeResult <- getArgs
  let input = map (read @([LatLong], [LatLong])) $ lines s
      arg = saveHead typeResult
  if arg == "csv"
    then LBS.putStrLn $ encodeCsvBS $ processCSV input
    else LBS.putStrLn $ XML.renderLBS XML.def $ processGpx input

saveHead :: [String] -> String
saveHead [] = ""
saveHead (a : _) = a

processGpx :: [([LatLong], [LatLong])] -> XML.Document
processGpx inp = doc
  where
    (beforeList, afterList) = unzip inp
    docBefore = batchesToGpxDoc "before" beforeList
    docAfter = batchesToGpxDoc "after" afterList
    mergedPoints = mergeWithPointsOfFirst docBefore docAfter
    doc = elementToDocument $ renderDoc mergedPoints

processCSV :: [([LatLong], [LatLong])] -> [CsvWaypoint]
processCSV inp = latLongToCsvWaypoint <$> fullList
  where
    (beforeList, afterList) = unzip inp
    fullList = concat (beforeList <> afterList)
