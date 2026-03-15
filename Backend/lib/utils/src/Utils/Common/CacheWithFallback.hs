{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Utility module that encapsulates the cache-then-DB fallback pattern.
-- Instead of each CachedQuery module manually implementing the
-- "check cache, on miss query DB, cache result" logic, callers can use
-- these helpers to get a single-call interface with an optional DB fallback.
module Utils.Common.CacheWithFallback
  ( findByIdWithFallback,
    findWithFallback,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common

-- | Look up a value by cache key. On cache miss, invoke the provided
-- fallback action (typically a DB query), cache the result, and return it.
-- This encapsulates the cache-then-DB pattern in one place, so callers
-- don't need to manually query the DB on cache miss.
findByIdWithFallback ::
  ( CacheFlow m r,
    ToJSON a,
    FromJSON a
  ) =>
  Text ->        -- ^ Cache key
  m (Maybe a) -> -- ^ Fallback action (e.g., DB query) invoked on cache miss
  m (Maybe a)
findByIdWithFallback cacheKey dbFallback = do
  Hedis.safeGet cacheKey >>= \case
    Just a -> pure (Just a)
    Nothing -> do
      result <- dbFallback
      whenJust result $ \val -> do
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.setExp cacheKey val expTime
      pure result

-- | Like 'findByIdWithFallback' but for list results.
-- Caches non-empty results; returns empty list on miss with no DB results.
findWithFallback ::
  ( CacheFlow m r,
    ToJSON a,
    FromJSON a
  ) =>
  Text ->      -- ^ Cache key
  m [a] ->     -- ^ Fallback action (e.g., DB query) invoked on cache miss
  m [a]
findWithFallback cacheKey dbFallback = do
  Hedis.safeGet cacheKey >>= \case
    Just a -> pure a
    Nothing -> do
      result <- dbFallback
      unless (null result) $ do
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.setExp cacheKey result expTime
      pure result
