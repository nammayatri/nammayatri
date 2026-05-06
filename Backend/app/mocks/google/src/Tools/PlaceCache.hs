{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Process-wide place ID → coordinates cache. Used so that
-- /place/autocomplete and /places/:autocomplete can hand out synthetic
-- placeIds whose coordinates the later /geocode/json (PlaceName)
-- request can resolve back. Keeps the rider-app's "user picks a
-- suggestion → server geocodes it → maps a route" flow self-consistent
-- without a real geocoder.
--
-- Bounded to MAX_ENTRIES so a long-lived dev session does not balloon.
-- Eviction = oldest insert wins (we just stop accepting new entries
-- once full); the cache is dev-only state, no need for LRU eligance.
module Tools.PlaceCache
  ( insertPlace,
    lookupPlace,
    encodeMockPlaceId,
    decodeMockPlaceId,
  )
where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Kernel.Prelude
import System.IO.Unsafe (unsafePerformIO)

type LatLon = (Double, Double)

maxEntries :: Int
maxEntries = 5000

{-# NOINLINE _cache #-}
_cache :: IORef (Map.Map Text LatLon)
_cache = unsafePerformIO (newIORef Map.empty)

-- | Build a deterministic placeId encoding the lat/lng. Round-trips
-- via decodeMockPlaceId so callers can either look up the cache OR
-- decode straight from the id string — both work, the cache is just
-- the canonical store.
encodeMockPlaceId :: Double -> Double -> Text
encodeMockPlaceId lat lon =
  "mock:" <> T.pack (show lat) <> ":" <> T.pack (show lon)

decodeMockPlaceId :: Text -> Maybe LatLon
decodeMockPlaceId pid =
  case T.splitOn ":" pid of
    ["mock", la, lo] -> (,) <$> readMaybe (T.unpack la) <*> readMaybe (T.unpack lo)
    _ -> Nothing

insertPlace :: Text -> LatLon -> IO ()
insertPlace pid latlon =
  atomicModifyIORef' _cache $ \m ->
    if Map.size m >= maxEntries
      then (m, ())
      else (Map.insert pid latlon m, ())

-- | Lookup order: in-memory cache → fall back to decoding the placeId
-- (mock:<lat>:<lon>). The latter handles cold-start cases where the
-- mock-google process restarted between autocomplete and place_details.
lookupPlace :: Text -> IO (Maybe LatLon)
lookupPlace pid = do
  m <- readIORef _cache
  case Map.lookup pid m of
    Just v -> pure (Just v)
    Nothing -> pure (decodeMockPlaceId pid)
