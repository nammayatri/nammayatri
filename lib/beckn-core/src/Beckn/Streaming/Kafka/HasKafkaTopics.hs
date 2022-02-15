{-# LANGUAGE AllowAmbiguousTypes #-}

module Beckn.Streaming.Kafka.HasKafkaTopics where

import Beckn.Streaming.Kafka.Commons

class HasKafkaTopics a where
  getTopics :: [KafkaTopic]
