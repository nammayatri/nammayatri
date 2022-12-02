module Service.Runner where

import Beckn.Prelude
import Beckn.Streaming.Kafka.Consumer
import Beckn.Streaming.Kafka.Topic.PublicTransportSearch
import Beckn.Streaming.MonadConsumer
import Beckn.Types.Logging
import Beckn.Utils.Common
import Control.Concurrent.STM.TMVar
import qualified Core.ACL.Search as BecknACL
import qualified Domain.Action.Search as DSearch
import qualified ExternalAPI.Flow as ExternalAPI
import GHC.Conc
import Tools.Metrics

run ::
  ( HasField "isShuttingDown" r (TMVar ()),
    HasField "bapId" r Text,
    HasField "bapURI" r BaseUrl,
    HasField "gatewayUrl" r BaseUrl,
    CoreMetrics m,
    HasHttpClientOptions r c,
    MonadConsumer PublicTransportSearch m,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  m ()
run = withLogTag "Service" $
  listenForMessages @PublicTransportSearch isRunning $ \searchReq -> withTransactionIdLogTag' searchReq.id $ do
    searchMessage <- DSearch.search searchReq
    becknSearchReq <- BecknACL.buildSearchReq searchMessage
    fork "search" . withRetry $ do
      -- do we need fork here?
      ExternalAPI.search becknSearchReq
  where
    isRunning = liftIO . atomically . isEmptyTMVar =<< asks (.isShuttingDown)
