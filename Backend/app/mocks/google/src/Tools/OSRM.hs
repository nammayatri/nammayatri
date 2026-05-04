{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Tiny OSRM client for the mock-google service. Used so the
-- mocked /directions and /distancematrix endpoints can return realistic
-- distance / duration / polyline values from the locally-running
-- osrm-routed (port 5001 by default) instead of straight-line estimates.
--
-- Every call returns Nothing on any failure (network error, OSRM down,
-- unparseable response). Callers MUST fall back to the existing mock
-- math so the dev experience does not depend on OSRM being healthy.
module Tools.OSRM
  ( getRoute,
    getTable,
    OsrmRoute (..),
    OsrmTable (..),
    osrmBaseUrl,
  )
where

import qualified Control.Exception as CE
import Data.Aeson (Value, decode, withObject, (.!=), (.:), (.:?))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BSL
import Data.List (intercalate)
import Kernel.Prelude
import Network.HTTP.Client
  ( defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
  )
import System.Environment (lookupEnv)

data OsrmRoute = OsrmRoute
  { distanceMeters :: Int,
    durationSeconds :: Int,
    polyline :: Text
  }
  deriving (Show, Eq)

data OsrmTable = OsrmTable
  { distancesMatrix :: [[Maybe Double]],
    durationsMatrix :: [[Maybe Double]]
  }
  deriving (Show, Eq)

-- | OSRM base URL. Reads $OSRM_HOST (e.g. http://localhost:5001) and
-- falls back to the local osrm-routed default port that
-- nix/services/nammayatri.nix wires up.
osrmBaseUrl :: IO String
osrmBaseUrl = do
  e <- lookupEnv "OSRM_HOST"
  pure $ fromMaybe "http://localhost:5001" e

-- | Single-leg driving route. OSRM returns its polyline pre-encoded in
-- the same format Google uses (precision-5 polyline algorithm), so we
-- pass the geometry string straight through to the response.
getRoute :: (Double, Double) -> (Double, Double) -> IO (Maybe OsrmRoute)
getRoute (lat1, lon1) (lat2, lon2) = do
  base <- osrmBaseUrl
  let url =
        base
          <> "/route/v1/driving/"
          <> show lon1
          <> ","
          <> show lat1
          <> ";"
          <> show lon2
          <> ","
          <> show lat2
          <> "?overview=full&geometries=polyline"
  res <- CE.try $ do
    mgr <- newManager defaultManagerSettings
    req <- parseRequest url
    resp <- httpLbs req mgr
    pure (responseBody resp)
  case (res :: Either CE.SomeException BSL.ByteString) of
    Left _ -> pure Nothing
    Right body -> pure (parseRoute body)

parseRoute :: BSL.ByteString -> Maybe OsrmRoute
parseRoute body = do
  parsed <- decode body >>= A.parseMaybe parser
  -- Sentinel for "OSRM accepted the request but couldn't route" — most
  -- commonly when the requested coords fall outside its loaded data
  -- bounding box. OSRM responds {code: Ok, routes: [{distance: 0,
  -- duration: 0, geometry: ""}]} in that case. Treating those as a
  -- successful zero-length route would lie to the caller; instead we
  -- return Nothing so callers fall back to their straight-line mock.
  if parsed.distanceMeters == 0 && parsed.durationSeconds == 0
    then Nothing
    else Just parsed
  where
    parser :: Value -> A.Parser OsrmRoute
    parser = withObject "OsrmResp" $ \o -> do
      routes <- o .: "routes" :: A.Parser [Value]
      case routes of
        (r : _) -> flip (withObject "Route") r $ \rt -> do
          dist <- rt .: "distance" :: A.Parser Double
          dur <- rt .: "duration" :: A.Parser Double
          geom <- rt .:? "geometry" .!= ""
          pure $ OsrmRoute (round dist) (round dur) geom
        [] -> fail "no routes"

-- | OSRM table service for an N×N distance + duration matrix.
-- ``coords`` are (lat, lon) pairs in row-major order. We always request
-- both annotations so the caller can pick whichever they need.
getTable :: [(Double, Double)] -> IO (Maybe OsrmTable)
getTable coords | null coords = pure Nothing
getTable coords = do
  base <- osrmBaseUrl
  let coordStr = intercalate ";" $ map (\(la, lo) -> show lo <> "," <> show la) coords
  let url = base <> "/table/v1/driving/" <> coordStr <> "?annotations=duration,distance"
  res <- CE.try $ do
    mgr <- newManager defaultManagerSettings
    req <- parseRequest url
    resp <- httpLbs req mgr
    pure (responseBody resp)
  case (res :: Either CE.SomeException BSL.ByteString) of
    Left _ -> pure Nothing
    Right body -> pure (parseTable body)

parseTable :: BSL.ByteString -> Maybe OsrmTable
parseTable body = do
  parsed <- decode body >>= A.parseMaybe parser
  -- Same out-of-bbox sentinel as parseRoute: when OSRM has no data for
  -- the area, every off-diagonal cell comes back as Just 0.0. We
  -- demote those to Nothing so callers can haversine-fallback per
  -- cell. Diagonal (i==i) is left as-is — a point's distance to
  -- itself is genuinely 0.
  let demote i j v
        | i /= j, v == Just 0.0 = Nothing
        | otherwise = v
      cleanRow i row = zipWith (demote i) [0 :: Int ..] row
      cleanRows rows = zipWith cleanRow [0 :: Int ..] rows
  pure $
    OsrmTable
      { distancesMatrix = cleanRows parsed.distancesMatrix,
        durationsMatrix = cleanRows parsed.durationsMatrix
      }
  where
    parser :: Value -> A.Parser OsrmTable
    parser = withObject "OsrmTableResp" $ \o -> do
      durs <- o .:? "durations" .!= []
      dists <- o .:? "distances" .!= []
      pure $ OsrmTable {distancesMatrix = dists, durationsMatrix = durs}
