module Storage.CachedQueries.CacheConfig where

import Beckn.Prelude
import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Types.Common
import Beckn.Utils.Dhall

newtype CacheConfig = CacheConfig
  { configsExpTime :: Seconds
  }
  deriving (Generic, FromDhall)

type HasCacheConfig r = HasField "cacheConfig" r CacheConfig

type CacheFlow m r = (HasCacheConfig r, HedisFlow m r)
