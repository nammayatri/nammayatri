{-# LANGUAGE AllowAmbiguousTypes #-}

module Beckn.Utils.CacheMVar where

import Beckn.Prelude
import Beckn.Types.Cache (CacheKey)
import Beckn.Types.Time
import Beckn.Utils.MVar
import Beckn.Utils.Time
import Control.Concurrent (ThreadId)
import qualified Data.Map as Map

newtype CacheMVar a = CacheMVar {cache :: MVar (Map.Map (CacheKey a) a)}

getKey ::
  ( MonadReader r m,
    MonadIO m,
    Ord (CacheKey a),
    HasField "cache" c (MVar (Map.Map (CacheKey a) a))
  ) =>
  (r -> c) ->
  CacheKey a ->
  m (Maybe a)
getKey cache key =
  asks cache
    >>= readMVar . (.cache)
    <&> Map.lookup key

setKey ::
  ( MonadReader r m,
    MonadIO m,
    Ord (CacheKey a),
    HasField "cache" c (MVar (Map.Map (CacheKey a) a))
  ) =>
  (r -> c) ->
  CacheKey a ->
  a ->
  m ()
setKey cache key val =
  asks cache
    >>= flip modifyMVar_' (Map.insert key val) . (.cache)

delKey ::
  ( MonadReader r m,
    MonadIO m,
    Ord (CacheKey a),
    HasField "cache" c (MVar (Map.Map (CacheKey a) a))
  ) =>
  (r -> c) ->
  CacheKey a ->
  m ()
delKey cache key =
  asks cache
    >>= flip modifyMVar_' (Map.delete key) . (.cache)

initSimpleCache :: IO (CacheMVar a)
initSimpleCache = CacheMVar <$> newMVar Map.empty

data CacheMVarEx a = CacheMVarEx
  { cache :: MVar (Map.Map (CacheKey a) a),
    rmQueue :: MVar (Map.Map UTCTime [CacheKey a]),
    threadId :: ThreadId -- not really used now
  }

setKeyEx ::
  ( MonadReader r m,
    MonadIO m,
    MonadTime m,
    Ord (CacheKey a)
  ) =>
  (r -> CacheMVarEx a) ->
  Seconds ->
  CacheKey a ->
  a ->
  m ()
setKeyEx cacheAccessor ttl key val = do
  setKey cacheAccessor key val
  addToRmQueue cacheAccessor ttl key

initCache :: Ord (CacheKey a) => IO (CacheMVarEx a)
initCache = do
  cache <- newMVar Map.empty
  rmQueue <- newMVar Map.empty
  threadId <- forkIO $ cacheRemover cache rmQueue
  pure CacheMVarEx {..}

cacheRemover ::
  ( MonadTime m,
    MonadIO m,
    Ord (CacheKey a)
  ) =>
  MVar (Map.Map (CacheKey a) a) ->
  MVar (Map.Map UTCTime [CacheKey a]) ->
  m b
cacheRemover cache rmQueue = forever do
  now <- getCurrentTime
  toRemove <-
    join <$> modifyMVar' rmQueue \queue ->
      let (map snd . Map.toList -> less, eq, greater) = Map.splitLookup now queue
       in (greater, maybe less (: less) eq)
  modifyMVar_' cache (`Map.difference` Map.fromList (map (,()) toRemove))
  liftIO . threadDelay . getMicroseconds $ secondsToMs cacheDelay

cacheDelay :: Seconds
cacheDelay = 5

addToRmQueue ::
  ( MonadTime m,
    MonadReader r m,
    MonadIO m
  ) =>
  (r -> CacheMVarEx a) ->
  Seconds ->
  CacheKey a ->
  m ()
addToRmQueue cacheAccessor ttl key = do
  expTime <- getCurrentTime <&> addUTCTime (fromIntegral ttl)
  CacheMVarEx {rmQueue} <- asks cacheAccessor
  modifyMVar_' rmQueue $ Map.adjust (key :) expTime
