module Beckn.Streaming.Kafka.Topic.PublicTransportSearch.Functions where

import Beckn.Streaming.Kafka.Topic.PublicTransportSearch.Types
import Beckn.Streaming.MonadProducer (MonadProducer (..))

producePublicTransportSearchMessage ::
  ( MonadProducer PublicTransportSearch m
  ) =>
  PublicTransportSearch ->
  m ()
producePublicTransportSearchMessage = do
  produceMessage ()
