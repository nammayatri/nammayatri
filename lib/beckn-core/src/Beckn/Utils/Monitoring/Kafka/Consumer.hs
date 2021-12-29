module Beckn.Utils.Monitoring.Kafka.Consumer
  ( Beckn.Utils.Monitoring.Kafka.Consumer.receiveMessage,
    listenForMessage,
  )
where

import Beckn.Types.App (MonadFlow)
import Beckn.Types.Error
import Beckn.Types.Forkable (fork)
import Beckn.Types.Monitoring.Kafka.Consumer as ConsTypes
import Beckn.Utils.Error.Throwing (throwError)
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
      case A.decode . LBS.fromStrict =<< crValue res of
        Nothing -> throwError KafkaUnableToParseValue
        Just a -> return $ Just a

listenForMessage :: (ConsTypes.KafkaConsumer a m, MonadFlow m, MonadReader r m, FromJSON a) => (a -> m ()) -> m ()
listenForMessage handle = fork "Listen for message" . forever $ do
  etrRes <- try @_ @SomeException ConsTypes.receiveMessage
  case etrRes of
    Left err -> logError $ "Message was not received: " <> show err
    Right res -> handle res
