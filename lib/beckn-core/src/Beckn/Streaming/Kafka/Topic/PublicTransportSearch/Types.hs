module Beckn.Streaming.Kafka.Topic.PublicTransportSearch.Types where

import Beckn.Prelude
import qualified Beckn.Streaming.Kafka.Consumer as Cons
import Beckn.Streaming.Kafka.Consumer.Types (HasKafkaConsumer, KafkaConsumerTools)
import Beckn.Streaming.Kafka.HasKafkaTopics (HasKafkaTopics (..))
import qualified Beckn.Streaming.Kafka.Producer as Prod
import Beckn.Streaming.Kafka.Producer.Types
import Beckn.Streaming.MonadConsumer (MonadConsumer (..))
import Beckn.Streaming.MonadProducer (MonadProducer (..))
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Logging
import Beckn.Types.MapSearch (LatLong)

type HasKafkaPublicTransportSearchConsumer env r =
  ( HasKafkaConsumer env r,
    HasField "publicTransportSearch" env (KafkaConsumerTools PublicTransportSearch)
  )

type SearchId = Text

type PersonId = Text

data PublicTransportSearch = PublicTransportSearch
  { id :: SearchId,
    gps :: LatLong,
    requestorId :: PersonId,
    createdAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON)

instance HasKafkaTopics PublicTransportSearch where
  getTopics = ["public_transport_bap_search"]

instance (Log (FlowR r), HasKafkaProducer r) => MonadProducer PublicTransportSearch (FlowR r) where
  type Args PublicTransportSearch = ()
  produceMessage () value = mapM_ ($ value) (Prod.produceMessage <$> map (,Nothing) (getTopics @PublicTransportSearch))

instance (Log (FlowR r), HasKafkaPublicTransportSearchConsumer env r) => MonadConsumer PublicTransportSearch (FlowR r) where
  receiveMessage = asks (.kafkaConsumerEnv.publicTransportSearch) >>= Cons.receiveMessage
