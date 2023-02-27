{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
