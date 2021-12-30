module Beckn.Streaming.MonadConsumer where

class MonadConsumer a m where
  receiveMessage :: m a
