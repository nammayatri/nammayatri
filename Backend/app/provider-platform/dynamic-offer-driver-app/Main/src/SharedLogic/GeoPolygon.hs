{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
  You should have received a copy of the GNU Affero General Public License along with this program, if not, see <https://www.gnu.org/licenses/>.
-}

-- | Pure geometry helpers: GeoJSON parsing and in-memory point-in-polygon.
--
-- These functions operate entirely on in-memory data; there are no DB or IO calls.
-- They are intentionally separate from StateEntryPermitDetector so other modules
-- (e.g. BlockedRouteDetector, TollsDetector) can reuse them without pulling in
-- SEPC-specific logic.
--
-- Coordinate convention (GeoJSON): coordinates are stored as [longitude, latitude].
-- LatLong uses .lat and .lon fields accordingly.
--
-- MultiPolygon GeoJSON structure (RFC 7946):
--   coordinates = [ polygon, ... ]
--   polygon     = [ ring, ... ]      -- ring[0] = exterior, ring[1..] = holes
--   ring        = [ [lon, lat], ... ]
module SharedLogic.GeoPolygon
  ( -- * Types
    Ring,
    GeoPolygon,
    MultiPolygon,

    -- * Parsing
    parseGeoJSONRings,

    -- * Point-in-polygon
    pointInRing,
    pointInPolygon,
    pointInMultiPolygon,
  )
where

import Data.Aeson (Value (..), decodeStrict, (.:))
import Data.Aeson.Types (parseMaybe)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A closed ring of lat/lon points (exterior boundary or hole).
type Ring = [LatLong]

-- | A polygon: first ring is the exterior boundary, remaining rings are holes.
-- Follows GeoJSON Polygon spec (RFC 7946 §3.1.6).
type GeoPolygon = [Ring]

-- | A list of polygons. Each polygon has its own exterior ring and optional holes.
-- Follows GeoJSON MultiPolygon spec (RFC 7946 §3.1.7).
type MultiPolygon = [GeoPolygon]

-- ---------------------------------------------------------------------------
-- GeoJSON parsing
-- ---------------------------------------------------------------------------

-- | Parse a GeoJSON geometry text (produced by PostGIS ST_AsGeoJSON) into
-- a MultiPolygon ring structure.
--
-- Handles both "Polygon" and "MultiPolygon" geometry types:
--   - "Polygon"      → wrapped in a single-element list to give [GeoPolygon]
--   - "MultiPolygon" → each element is a GeoPolygon
--
-- Returns Nothing on malformed input (wrong type, missing fields, bad coordinates).
-- Points with fewer than 2 coordinates are skipped silently (GeoJSON allows Z/M dims).
parseGeoJSONRings :: Text -> Maybe MultiPolygon
parseGeoJSONRings geomText = do
  val <- decodeStrict (encodeUtf8 geomText)
  case val of
    Object obj -> do
      gtype <- parseMaybe (.: "type") obj :: Maybe Text
      case gtype of
        "Polygon" -> do
          rawRings <- parseMaybe (.: "coordinates") obj :: Maybe [[[Double]]]
          rings <- mapM parseRing rawRings
          pure [rings]
        "MultiPolygon" -> do
          rawPolygons <- parseMaybe (.: "coordinates") obj :: Maybe [[[[Double]]]]
          mapM (mapM parseRing) rawPolygons
        _ -> Nothing
    _ -> Nothing
  where
    parsePoint :: [Double] -> Maybe LatLong
    parsePoint (lon : lat : _) = Just LatLong {lat = lat, lon = lon}
    parsePoint _ = Nothing

    parseRing :: [[Double]] -> Maybe Ring
    parseRing = mapM parsePoint

-- ---------------------------------------------------------------------------
-- Point-in-polygon (ray-casting / even-odd rule)
-- ---------------------------------------------------------------------------

-- | True if the point is strictly inside the ring (even-odd ray-casting).
--
-- Shoots a ray in the +longitude direction and counts edge crossings.
-- Division by zero is avoided by short-circuit evaluation: the horizontal-edge
-- check `(cur.lat > pt.lat) /= (prev.lat > pt.lat)` is False when both
-- endpoints share the same latitude, so the division is never reached.
--
-- Safe on empty and single-point rings (returns False).
-- Points exactly on an edge return False (strict inequalities); this is acceptable
-- for GPS data where exact boundary hits have negligible probability.
pointInRing :: LatLong -> Ring -> Bool
pointInRing _ [] = False
pointInRing _ [_] = False
pointInRing pt ring = go False (last ring) ring
  where
    go inside _ [] = inside
    go inside prev (cur : rest) =
      let crosses =
            (cur.lat > pt.lat) /= (prev.lat > pt.lat)
              && pt.lon
                < (prev.lon - cur.lon) * (pt.lat - cur.lat)
                  / (prev.lat - cur.lat)
                  + cur.lon
       in go (if crosses then not inside else inside) cur rest

-- | True if the point is inside the polygon (exterior ring) and outside all holes.
--
-- GeoJSON polygon ring layout: ring[0] = exterior, ring[1..] = holes.
-- A point inside a hole is NOT inside the polygon.
pointInPolygon :: LatLong -> GeoPolygon -> Bool
pointInPolygon _ [] = False
pointInPolygon pt (exterior : holes) =
  pointInRing pt exterior && not (any (pointInRing pt) holes)

-- | True if the point is inside any polygon of the multi-polygon.
pointInMultiPolygon :: LatLong -> MultiPolygon -> Bool
pointInMultiPolygon pt = any (pointInPolygon pt)
