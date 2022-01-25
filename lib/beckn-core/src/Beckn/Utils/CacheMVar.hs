{-# LANGUAGE AllowAmbiguousTypes #-}

module Beckn.Utils.CacheMVar where

import Beckn.Prelude
import Control.Concurrent.MVar
import qualified Data.Map as Map

type CacheMVar k a = MVar (Map.Map k a)

getKey ::
  ( MonadReader r m,
    MonadIO m,
    Ord k
  ) =>
  (r -> CacheMVar k a) ->
  k ->
  m (Maybe a)
getKey cache key =
  asks cache
    >>= liftIO . readMVar
    <&> Map.lookup key

setKey ::
  ( MonadReader r m,
    MonadIO m,
    Ord k
  ) =>
  (r -> CacheMVar k a) ->
  k ->
  a ->
  m ()
setKey cache key val =
  asks
    cache
    >>= liftIO . flip modifyMVar_ (pure . Map.insert key val)

delKey ::
  ( MonadReader r m,
    MonadIO m,
    Ord k
  ) =>
  (r -> CacheMVar k a) ->
  k ->
  m ()
delKey cache key =
  asks cache
    >>= liftIO . flip modifyMVar_ (pure . Map.delete key)

initCache :: IO (CacheMVar k a)
initCache = newMVar Map.empty
