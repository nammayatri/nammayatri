module Service.Runner where

import Beckn.Prelude
import Beckn.Storage.Hedis as Hedis
import Beckn.Streaming.Kafka.Consumer
import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList
import Beckn.Streaming.MonadConsumer
import Beckn.Types.App (MonadFlow)
import Beckn.Types.Logging
import Beckn.Utils.Logging
import Control.Concurrent.STM.TMVar
import GHC.Conc

run ::
  ( HasField "isShuttingDown" r (TMVar ()),
    MonadConsumer PublicTransportQuoteList m,
    HedisFlow m r,
    MonadFlow m
  ) =>
  m ()
run = do
  withLogTag "Service" $ do
    listenForMessages @PublicTransportQuoteList isRunning $ \PublicTransportQuoteList {..} ->
      withTransactionIdLogTag' transactionId $
        Hedis.rPushExp (makeHedisKey transactionId) quoteList expirationTime
  where
    makeHedisKey transactionId = "publicTransportQuoteList:" <> transactionId
    expirationTime = 600
    isRunning = liftIO . atomically . isEmptyTMVar =<< asks (.isShuttingDown)
