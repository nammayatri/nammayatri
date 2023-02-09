module Service.Runner where

import Control.Concurrent.STM.TMVar
import GHC.Conc
import Kernel.Prelude
import Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Consumer
import Kernel.Streaming.Kafka.Topic.PublicTransportQuoteList (PublicTransportQuoteList (..))
import Kernel.Streaming.MonadConsumer
import Kernel.Types.App (MonadFlow)
import Kernel.Types.Id
import Kernel.Types.Logging
import Kernel.Utils.Logging
import qualified SharedLogic.PublicTransport as PublicTransport

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
        PublicTransport.cachePublicTransportOffers (Id transactionId) quoteList
  where
    isRunning = liftIO . atomically . isEmptyTMVar =<< asks (.isShuttingDown)
