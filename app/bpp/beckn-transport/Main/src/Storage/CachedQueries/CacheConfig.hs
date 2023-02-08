module Storage.CachedQueries.CacheConfig where

import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common
import Kernel.Utils.Dhall

newtype CacheConfig = CacheConfig
  { configsExpTime :: Seconds
  }
  deriving (Generic, FromDhall)

type HasCacheConfig r = HasField "cacheConfig" r CacheConfig

type CacheFlow m r = (HasCacheConfig r, HedisFlow m r)
