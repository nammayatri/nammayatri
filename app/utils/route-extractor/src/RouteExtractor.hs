{-# OPTIONS_GHC -Wno-orphans #-}

module RouteExtractor where

import Beckn.Prelude
import Beckn.Types.MapSearch (LatLong (LatLong))
import qualified Data.ByteString.Lazy.Char8 as LBS
import RenderXml
import qualified Text.XML as XML

deriving instance Read LatLong

-- we don't want to ever use Read instance in production code,
-- so let this instance be here

main :: IO ()
main = do
  s <- getContents
  let input = map (read @([LatLong], [LatLong])) $ lines s
      doc = process input
      output = XML.renderLBS XML.def doc
  LBS.putStrLn output

process :: [([LatLong], [LatLong])] -> XML.Document
process inp = do
  let (beforeList, afterList) = unzip inp
      docBefore = batchesToGpxDoc "before" beforeList
      docAfter = batchesToGpxDoc "after" afterList
  elementToDocument $ renderDoc $ mergeWithPointsOfFirst docBefore docAfter
