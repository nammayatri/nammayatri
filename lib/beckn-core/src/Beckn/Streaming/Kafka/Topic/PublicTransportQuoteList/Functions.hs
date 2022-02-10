module Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList.Functions where

import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList.Types
import Beckn.Streaming.MonadProducer (MonadProducer (..))
import EulerHS.Prelude

producePublicTransportQuoteListMessage ::
  ( MonadProducer PublicTransportQuoteList m
  ) =>
  TransactionId ->
  [PublicTransportQuote] ->
  m ()
producePublicTransportQuoteListMessage txnId = do
  produceMessage () . PublicTransportQuoteList txnId
