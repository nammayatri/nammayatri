module Beckn.Streaming.Kafka.Commons where

import EulerHS.Prelude

type KafkaBrokerAddress = Text

type KafkaBrokersList = [KafkaBrokerAddress]

type KafkaTopic = Text

type KafkaKey = ByteString

type KafkaHostName = Maybe Text

type KafkaServiceName = Text
