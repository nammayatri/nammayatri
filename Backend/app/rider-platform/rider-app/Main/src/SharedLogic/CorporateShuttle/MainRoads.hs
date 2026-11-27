{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bangalore's main roads, from OpenStreetMap (the southern-zone extract the local road network is built from, January
-- 2024): every trunk, primary, secondary and tertiary road at ground level (no flyovers, underpasses or ramps, where a
-- shuttle cannot stop), with its name. A road is main by the road class OpenStreetMap gives it, never by its name.
--
-- data/corporate-shuttle/bangalore-main-roads.txt has one road per line: class (4 trunk, primary or secondary; 3
-- tertiary), name, and its points as lat*1e5-1200000,lon*1e5-7700000 separated by ';'.
module SharedLogic.CorporateShuttle.MainRoads
  ( MainRoadPoint (..),
    mainRoadAt,
    mainRoadPointsNear,
    nearestMainRoad,
    pointsOnMainRoad,
    densify,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BSU
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified SharedLogic.CorporateShuttle.RoutePlanner as RP
import System.IO.Unsafe (unsafePerformIO)

-- | A point on a main road: where, its class (4 trunk, primary or secondary; 3 tertiary) and the road's name.
data MainRoadPoint = MainRoadPoint {roadPointAt :: LatLong, roadPointClass :: Int, roadPointName :: Maybe Text}

-- | A short stretch of a main road (at most pieceMeters long).
data Piece = Piece {pieceClass :: Int, pieceName :: Maybe Text, pieceA :: LatLong, pieceB :: LatLong}

roadData :: BS.ByteString
roadData =
  $( do
       let path = "data/corporate-shuttle/bangalore-main-roads.txt"
       TH.addDependentFile path
       bytes <- TH.runIO (BS.readFile path)
       -- unsafePerformIO (BSU.unsafePackAddressLen <length> "<bytes>"#), built without a [| |] quote
       TH.appE
         (TH.varE 'unsafePerformIO)
         (TH.appsE [TH.varE 'BSU.unsafePackAddressLen, TH.lift (BS.length bytes), TH.litE (TH.stringPrimL (BS.unpack bytes))])
   )
{-# NOINLINE roadData #-}

pieceMeters, cellDeg :: Double
pieceMeters = 50
cellDeg = 0.005

-- | The roads, cut into pieces, by grid cell (read once, on first use).
index :: Map.Map (Int, Int) [Piece]
index = Map.fromListWith (<>) [(cellOf (midOf p), [p]) | p <- concatMap parseLine (BC.lines roadData)]
  where
    parseLine l = case BC.split '|' l of
      [c, n, ps] ->
        let cls = maybe 3 fst (BC.readInt c)
            name = mfilter (not . T.null) (Just (T.strip (TE.decodeUtf8With TEE.lenientDecode n)))
            pts = mapMaybe point (BC.split ';' ps)
         in concat (zipWith (cut cls name) pts (drop 1 pts))
      _ -> []
    point s = case BC.split ',' s of
      [y, x] -> do
        (yi, _) <- BC.readInt y
        (xi, _) <- BC.readInt x
        pure (LatLong ((fromIntegral yi + 1200000) / 1e5) ((fromIntegral xi + 7700000) / 1e5))
      _ -> Nothing
    cut cls name a b =
      let n = max 1 (ceiling (RP.metersBetween a b / pieceMeters)) :: Int
          at' i = let t = fromIntegral i / fromIntegral n in LatLong (a.lat + t * (b.lat - a.lat)) (a.lon + t * (b.lon - a.lon))
       in [Piece cls name (at' i) (at' (i + 1)) | i <- [0 .. n - 1]]
{-# NOINLINE index #-}

midOf :: Piece -> LatLong
midOf p = LatLong ((p.pieceA.lat + p.pieceB.lat) / 2) ((p.pieceA.lon + p.pieceB.lon) / 2)

cellOf :: LatLong -> (Int, Int)
cellOf p = (floor (p.lat / cellDeg), floor (p.lon / cellDeg))

-- | The pieces within metres of a point, each with the nearest point on it and how far that is.
piecesNear :: Double -> LatLong -> [(Double, LatLong, Piece)]
piecesNear metres p =
  [ (d, q, pc)
    | dy <- [- span' .. span'],
      dx <- [- span' .. span'],
      pc <- Map.findWithDefault [] (cy + dy, cx + dx) index,
      let (d, q) = toPiece p pc,
      d <= metres
  ]
  where
    (cy, cx) = cellOf p
    span' = ceiling (metres / (cellDeg * 108000)) + 1 :: Int

toPiece :: LatLong -> Piece -> (Double, LatLong)
toPiece q pc =
  let kx = 111320 * cos (q.lat * pi / 180)
      ky = 110540
      xy p = ((p.lon - q.lon) * kx, (p.lat - q.lat) * ky)
      (ax, ay) = xy pc.pieceA
      (bx, by) = xy pc.pieceB
      (dx, dy) = (bx - ax, by - ay)
      len2 = dx * dx + dy * dy
      t = if len2 == 0 then 0 else max 0 (min 1 (negate (ax * dx + ay * dy) / len2))
      (px, py) = (ax + t * dx, ay + t * dy)
   in (sqrt (px * px + py * py), LatLong (q.lat + py / ky) (q.lon + px / kx))

-- | The main road a point is on (within 25 m of it), if any.
mainRoadAt :: LatLong -> Maybe MainRoadPoint
mainRoadAt p = case sortOn (\(d, _, pc) -> (d, negate pc.pieceClass)) (piecesNear 25 p) of
  (_, q, pc) : _ -> Just (MainRoadPoint q pc.pieceClass pc.pieceName)
  [] -> Nothing

-- | The nearest point on a main road within metres, if any.
nearestMainRoad :: Double -> LatLong -> Maybe MainRoadPoint
nearestMainRoad metres p = case sortOn (\(d, _, _) -> d) (piecesNear metres p) of
  (_, q, pc) : _ -> Just (MainRoadPoint q pc.pieceClass pc.pieceName)
  [] -> Nothing

-- | Main-road points (one about every pieceMeters along each road) within metres (straight line) of any of the points.
mainRoadPointsNear :: Double -> [LatLong] -> [MainRoadPoint]
mainRoadPointsNear metres ps =
  Map.elems
    ( Map.fromList
        [ (key pc.pieceA, MainRoadPoint pc.pieceA pc.pieceClass pc.pieceName)
          | p <- ps,
            let (y, x) = cellOf p,
            dy <- [- span' .. span'],
            dx <- [- span' .. span'],
            pc <- Map.findWithDefault [] (y + dy, x + dx) index,
            RP.metersBetween p pc.pieceA <= metres
        ]
    )
  where
    span' = ceiling (metres / (cellDeg * 108000)) + 1 :: Int
    key q = (round (q.lat * 1e5) :: Integer, round (q.lon * 1e5) :: Integer)

-- | Points along a road line (one about every 25 m) that are on a main road, each with the road.
pointsOnMainRoad :: [LatLong] -> [(Int, LatLong, MainRoadPoint)]
pointsOnMainRoad line =
  [(i, q, r) | (i, q) <- zip [0 ..] (densify line), Just r <- [mainRoadAt q]]

-- | A line with a point at least every 25 m.
densify :: [LatLong] -> [LatLong]
densify line = take 1 line <> concat (zipWith between line (drop 1 line))
  where
    between a b =
      let n = max 1 (ceiling (RP.metersBetween a b / 25)) :: Int
       in [LatLong (a.lat + t * (b.lat - a.lat)) (a.lon + t * (b.lon - a.lon)) | i <- [1 .. n], let t = fromIntegral i / fromIntegral n]
