{-# LANGUAGE AllowAmbiguousTypes #-}

module Beckn.Types.Cache where

import Beckn.Prelude
import Beckn.Types.Time (Seconds)

class Cache a m where
  type CacheKey a
  getKey :: CacheKey a -> m (Maybe a)
  setKey :: CacheKey a -> a -> m ()
  delKey :: CacheKey a -> m ()

class Cache a m => CacheEx a m where
  setKeyEx :: Seconds -> CacheKey a -> a -> m ()

caching ::
  (CacheEx a m, Monad m) =>
  (a -> Seconds) ->
  (CacheKey a -> m (Maybe a)) ->
  CacheKey a ->
  m (Maybe a)
caching getTtl getData key =
  getKey key >>= \case
    Nothing -> do
      mbRes <- getData key
      whenJust mbRes \res -> setKeyEx (getTtl res) key res
      pure mbRes
    res -> pure res

withCaching ::
  (Cache a m, Monad m) => CacheKey a -> m a -> m a
withCaching key mValue = do
  value <- mValue
  setKey key value
  return value
