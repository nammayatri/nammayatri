module Beckn.Streaming.MonadConsumer where

import Beckn.Prelude

class MonadConsumer a m where
  receiveMessage :: m (Maybe a)
