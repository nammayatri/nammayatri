module Beckn.Streaming.MonadProducer where

class MonadProducer a m where
  type Args a
  produceMessage :: Args a -> a -> m ()
