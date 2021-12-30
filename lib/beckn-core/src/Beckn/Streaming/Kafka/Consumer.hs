module Beckn.Streaming.Kafka.Consumer
  ( Beckn.Streaming.Kafka.Consumer.receiveMessage,
    listenForMessage,
  )
where

import Beckn.Streaming.Kafka.Consumer.Types as ConsTypes
import qualified Beckn.Streaming.MonadConsumer as MonadCons
import Beckn.Types.App (MonadFlow)
import Beckn.Types.Error
import Beckn.Types.Forkable (fork)
import Beckn.Utils.Error.Throwing (fromMaybeM)
import Beckn.Utils.Logging
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import EulerHS.Prelude
import Kafka.Consumer as KafkaCons

receiveMessage :: (MonadFlow m, MonadReader r m, FromJSON a) => KafkaConsumerTools a -> m (Maybe a)
receiveMessage kafkaConsumerTools = withLogTag "KafkaConsumer" $ do
  etrMsg <- pollMessage kafkaConsumerTools.consumer (Timeout 1000)
  mbErr <- commitAllOffsets OffsetCommit kafkaConsumerTools.consumer
  whenJust mbErr $ \err -> logError $ "Unable to commit offsets: " <> show err
  case etrMsg of
    Left err -> do
      logError $ "Unable to poll for message: " <> show err
      return Nothing
    Right res -> do
      crValue res >>= A.decode . LBS.fromStrict
        & fromMaybeM KafkaUnableToParseValue
        <&> Just

listenForMessage :: (MonadCons.MonadConsumer a m, MonadFlow m, MonadReader r m, FromJSON a) => (a -> m ()) -> m ()
listenForMessage handle = fork "Listen for message" . forever $ do
  etrRes <- try @_ @SomeException MonadCons.receiveMessage
  case etrRes of
    Left err -> logError $ "Message was not received: " <> show err
    Right res -> handle res
