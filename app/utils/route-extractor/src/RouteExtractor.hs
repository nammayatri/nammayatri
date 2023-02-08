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
