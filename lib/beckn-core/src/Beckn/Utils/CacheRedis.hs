{-# LANGUAGE AllowAmbiguousTypes #-}

module Beckn.Utils.CacheRedis
  ( getKey,
    setKey,
    delKey,
  )
where

import Beckn.Prelude
import Beckn.Storage.Redis.Queries
import Beckn.Types.Common

getKey ::
  ( HasCallStack,
    MonadFlow m,
    FromJSON a
  ) =>
  Text ->
  Text ->
  m (Maybe a)
getKey prefix key = getKeyRedis (mkKey prefix key)

setKey ::
  ( HasCallStack,
    MonadFlow m,
    ToJSON a
  ) =>
  Text ->
  Text ->
  a ->
  m ()
setKey prefix key = setKeyRedis (mkKey prefix key)

delKey ::
  (HasCallStack, MonadFlow m) =>
  Text ->
  Text ->
  m ()
delKey prefix key = void $ deleteKeyRedis (mkKey prefix key)

mkKey :: Text -> Text -> Text
mkKey prefix key = prefix <> ":" <> key
