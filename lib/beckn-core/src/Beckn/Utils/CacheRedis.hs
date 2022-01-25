{-# LANGUAGE AllowAmbiguousTypes #-}

module Beckn.Utils.CacheRedis
  ( getKey,
    setKey,
    delKey,
    setKeyEx,
  )
where

import Beckn.Prelude
import Beckn.Storage.Redis.Queries
import Beckn.Types.Common

type Key = Text

getKey ::
  ( HasCallStack,
    MonadFlow m,
    FromJSON a
  ) =>
  Text ->
  Key ->
  m (Maybe a)
getKey prefix key = getKeyRedis (mkKey prefix key)

setKey ::
  ( HasCallStack,
    MonadFlow m,
    ToJSON a
  ) =>
  Text ->
  Key ->
  a ->
  m ()
setKey prefix key = setKeyRedis (mkKey prefix key)

setKeyEx ::
  ( HasCallStack,
    MonadFlow m,
    ToJSON a
  ) =>
  Text ->
  Seconds ->
  Key ->
  a ->
  m ()
setKeyEx prefix ttl key val = setExRedis (mkKey prefix key) val ttl.getSeconds

delKey ::
  (HasCallStack, MonadFlow m) =>
  Text ->
  Key ->
  m ()
delKey prefix key = void $ deleteKeyRedis (mkKey prefix key)

mkKey :: Text -> Key -> Key
mkKey prefix key = prefix <> ":" <> key
