module Beckn.Streaming.Kafka.Consumer
  ( Beckn.Streaming.Kafka.Consumer.receiveMessage,
    listenForMessages,
  )
where

import Beckn.Prelude
import Beckn.Streaming.Kafka.Consumer.Types as ConsTypes
import qualified Beckn.Streaming.MonadConsumer as MonadCons
import Beckn.Types.Error
import Beckn.Utils.Error.Throwing (fromMaybeM, throwError)
import Beckn.Utils.Logging
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Kafka.Consumer as KafkaCons

receiveMessage :: (MonadIO m, Log m, MonadThrow m, FromJSON a) => KafkaConsumerTools a -> m (Maybe a)
receiveMessage kafkaConsumerTools = withLogTag "KafkaConsumer" $ do
  etrMsg <- pollMessage kafkaConsumerTools.consumer (Timeout 10000)
  case etrMsg of
    Left err -> handleResponseError err
    Right res -> do
      mbErr <- commitAllOffsets OffsetCommit kafkaConsumerTools.consumer
      whenJust mbErr $ \err -> logError $ "Unable to commit offsets: " <> show err
      crValue res >>= A.decode . LBS.fromStrict
        & fromMaybeM KafkaUnableToParseValue
        <&> Just
  where
    handleResponseError err =
      case err of
        KafkaResponseError RdKafkaRespErrTimedOut -> do
          logInfo "No messages to consume."
          return Nothing
        _ -> throwError $ KafkaUnableToConsumeMessage err

listenForMessages ::
  ( MonadCons.MonadConsumer a m,
    MonadIO m,
    MonadCatch m,
    Log m,
    MonadThrow m
  ) =>
  m Bool ->
  (a -> m ()) ->
  m ()
listenForMessages isRunning handle = whileM isRunning $ do
  etrRes <- try @_ @SomeException MonadCons.receiveMessage
  case etrRes of
    Left err -> logInfo $ "Message was not received: " <> show err
    Right res -> forM_ res handle
