{-# LANGUAGE AllowAmbiguousTypes #-}

module Beckn.Types.Cache where

import Beckn.Prelude

class Cache a m where
  type CacheKey a
  getKey :: CacheKey a -> m (Maybe a)
  setKey :: CacheKey a -> a -> m ()
  delKey :: CacheKey a -> m ()

caching ::
  (Cache a m, Monad m) =>
  (CacheKey a -> m (Maybe a)) ->
  CacheKey a ->
  m (Maybe a)
caching getData key =
  getKey key >>= \case
    Nothing -> do
      res <- getData key
      whenJust res (setKey key)
      pure res
    res -> pure res
