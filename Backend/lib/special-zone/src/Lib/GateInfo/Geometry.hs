{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | App-side geometry utilities for gate pickup zones. These replace the PostGIS
--   @ST_Contains@ / @ST_AsGeoJSON@ round-trips that the gate flows previously
--   relied on: the pickup-zone polygon is stored as GeoJSON text (the
--   @geom_geojson@ column) and all containment / proximity checks run in Haskell
--   over an in-memory cached, parsed polygon. No Postgres geometry ops on the hot
--   path.
module Lib.GateInfo.Geometry
  ( CachedGateForProximity (..),
    parseGatePolygons,
    pointInRing,
    pointInPolygon,
    pointToSegmentMeters,
    minDistanceToPolygonEdges,
    isPointInOrNearGate,
    getGeoJsonFromKML,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified EulerHS.Language as L
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common (MonadFlow)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process (callCommand)

-- | Cached snapshot of a queueable gate used for the proximity check. Carries the
--   parsed pickup-zone polygon(s) so the in-memory filter can do point-in-polygon
--   and edge-distance checks without hitting Postgres on the hot path.
--   `polygons` is empty when the gate has no/invalid geom — in that case the
--   filter falls back to a haversine check around `centerPoint`.
data CachedGateForProximity = CachedGateForProximity
  { gateId :: Text,
    canQueueUpOnGate :: Bool,
    centerPoint :: LatLong,
    polygons :: [[[LatLong]]] -- list of polygons; each polygon is rings (outer first, then holes); each ring is closed.
  }
  deriving (Show, Generic, FromJSON, ToJSON, Typeable)

-- | Parse a GeoJSON geometry (Polygon or MultiPolygon — e.g. the output of
--   @ST_AsGeoJSON@ stored in @geom_geojson@) into a list of polygons. Each polygon
--   is a list of closed rings (outer + holes); each ring is a list of LatLong
--   (note GeoJSON stores [lon, lat]).
parseGatePolygons :: Text -> Maybe [[[LatLong]]]
parseGatePolygons txt = do
  v <- either (const Nothing) Just (A.eitherDecodeStrict (TE.encodeUtf8 txt))
  AT.parseMaybe parseShape v
  where
    parseShape :: A.Value -> AT.Parser [[[LatLong]]]
    parseShape = A.withObject "Geometry" $ \o -> do
      typ :: Text <- o A..: "type"
      coords <- o A..: "coordinates"
      case typ of
        "Polygon" -> do
          poly <- parsePolygon coords
          pure [poly]
        "MultiPolygon" -> A.withArray "MultiPolygonCoords" (mapM parsePolygon . V.toList) coords
        _ -> fail $ "Unsupported geometry type: " <> show typ

parsePolygon :: A.Value -> AT.Parser [[LatLong]]
parsePolygon = A.withArray "PolygonCoords" $ \rings -> mapM parseRing (V.toList rings)

parseRing :: A.Value -> AT.Parser [LatLong]
parseRing = A.withArray "Ring" $ \pts -> mapM parsePoint (V.toList pts)

parsePoint :: A.Value -> AT.Parser LatLong
parsePoint = A.withArray "Coord" $ \pair -> do
  coords :: [Double] <- mapM AT.parseJSON (V.toList pair)
  case coords of
    (lon : lat : _) -> pure $ LatLong lat lon
    _ -> fail "Expected [lon, lat] coordinate"

-- | Ray-casting point-in-ring test. Assumes a closed ring (last == first).
pointInRing :: LatLong -> [LatLong] -> Bool
pointInRing _ ring | length ring < 4 = False
pointInRing (LatLong y x) ring =
  foldl' step False (zip ring (drop 1 ring))
  where
    step inside (LatLong y1 x1, LatLong y2 x2)
      | y1 == y2 = inside -- horizontal edge: ignore (avoids div-by-zero)
      | ((y1 > y) /= (y2 > y))
          && (x < (x2 - x1) * (y - y1) / (y2 - y1) + x1) =
        not inside
      | otherwise = inside

-- | Inside the polygon (outer ring AND not in any hole).
pointInPolygon :: LatLong -> [[LatLong]] -> Bool
pointInPolygon _ [] = False
pointInPolygon p (outer : holes) = pointInRing p outer && not (any (pointInRing p) holes)

-- | Approximate point-to-segment distance in meters using equirectangular projection.
--   Fine for radii <<< Earth radius (we use ~150m here).
pointToSegmentMeters :: LatLong -> LatLong -> LatLong -> Double
pointToSegmentMeters p a b =
  let latRad = (a.lat + b.lat) / 2 * pi / 180
      mPerDegLat = 111320.0 :: Double
      mPerDegLon = 111320.0 * cos latRad
      ax = a.lon * mPerDegLon
      ay = a.lat * mPerDegLat
      bx = b.lon * mPerDegLon
      by = b.lat * mPerDegLat
      px = p.lon * mPerDegLon
      py = p.lat * mPerDegLat
      dx = bx - ax
      dy = by - ay
      lenSq = dx * dx + dy * dy
      t = if lenSq <= 0 then 0 else max 0 (min 1 (((px - ax) * dx + (py - ay) * dy) / lenSq))
      cx = ax + t * dx
      cy = ay + t * dy
   in sqrt ((px - cx) * (px - cx) + (py - cy) * (py - cy))

-- | Min distance from point to any edge of any ring of the polygon (meters).
minDistanceToPolygonEdges :: LatLong -> [[LatLong]] -> Double
minDistanceToPolygonEdges _ [] = 1 / 0
minDistanceToPolygonEdges p rings =
  minimum $
    map
      (\ring -> if length ring < 2 then 1 / 0 else minimum (zipWith (pointToSegmentMeters p) ring (drop 1 ring)))
      rings

-- | True iff the driver is inside any of the gate's polygons OR within `radius`
--   meters of any polygon's edge. Falls back to haversine-from-centerPoint when
--   the gate has no parsed polygons.
isPointInOrNearGate :: LatLong -> Double -> CachedGateForProximity -> Bool
isPointInOrNearGate p radius gate
  | null gate.polygons =
    realToFrac (distanceBetweenInMeters p gate.centerPoint) < radius
  | otherwise =
    any (\poly -> pointInPolygon p poly || minDistanceToPolygonEdges p poly < radius) gate.polygons

-- | Convert a KML file (gate pickup zone) into GeoJSON @MultiPolygon@ text suitable
--   for the @geom_geojson@ column. Uses @ogr2ogr@ to read the KML into GeoJSON,
--   then collapses every feature's polygon(s) into a single MultiPolygon (2D).
--   This replaces the old PostGIS write path (KML -> shapefile -> @geom@ column);
--   the produced text round-trips through 'parseGatePolygons'.
getGeoJsonFromKML :: (MonadFlow m) => FilePath -> m (Maybe Text)
getGeoJsonFromKML kmlFilePath = do
  currentDir <- L.runIO getCurrentDirectory
  let tempDirPath = currentDir </> "temp_gate_geojson"
      geoJsonFilePath = tempDirPath </> "gate_output.json"
  L.runIO $ do
    removeDirectoryRecursiveIfExists tempDirPath
    createDirectoryIfMissing True tempDirPath
  let kmlToGeoJson = "ogr2ogr -f GeoJSON " ++ geoJsonFilePath ++ " " ++ kmlFilePath
  _ <- L.runIO $ callCommand kmlToGeoJson
  mbVal <- L.runIO $ A.decodeFileStrict' geoJsonFilePath
  L.runIO $ removeDirectoryRecursiveIfExists tempDirPath
  pure $ mbVal >>= featureCollectionToMultiPolygon
  where
    removeDirectoryRecursiveIfExists :: FilePath -> IO ()
    removeDirectoryRecursiveIfExists path = do
      exists <- doesDirectoryExist path
      when exists $ removeDirectoryRecursive path

-- | Extract all polygons from an ogr2ogr GeoJSON FeatureCollection and re-encode
--   them as a single GeoJSON MultiPolygon (dropping any altitude / 3rd ordinate).
--   Returns Nothing if no polygonal geometry is present.
featureCollectionToMultiPolygon :: A.Value -> Maybe Text
featureCollectionToMultiPolygon v = do
  polys <- AT.parseMaybe parseFeatureCollection v
  if null polys
    then Nothing
    else Just $ encodeMultiPolygon (concat polys)
  where
    parseFeatureCollection :: A.Value -> AT.Parser [[[[LatLong]]]]
    parseFeatureCollection = A.withObject "FeatureCollection" $ \o -> do
      features <- o A..: "features"
      A.withArray "features" (mapM parseFeature . V.toList) features

    parseFeature :: A.Value -> AT.Parser [[[LatLong]]]
    parseFeature = A.withObject "Feature" $ \o -> do
      geom <- o A..: "geometry"
      parseFeatureGeom geom

    parseFeatureGeom :: A.Value -> AT.Parser [[[LatLong]]]
    parseFeatureGeom = A.withObject "Geometry" $ \o -> do
      typ :: Text <- o A..: "type"
      coords <- o A..: "coordinates"
      case typ of
        "Polygon" -> (: []) <$> parsePolygon coords
        "MultiPolygon" -> A.withArray "MultiPolygonCoords" (mapM parsePolygon . V.toList) coords
        _ -> pure []

encodeMultiPolygon :: [[[LatLong]]] -> Text
encodeMultiPolygon polys =
  TE.decodeUtf8 . BL.toStrict . A.encode $
    A.object
      [ "type" A..= ("MultiPolygon" :: Text),
        "coordinates" A..= map (map (map (\ll -> [ll.lon, ll.lat]))) polys
      ]
