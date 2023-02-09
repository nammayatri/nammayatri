module Service.Runner where

import qualified Beckn.ACL.Search as BecknACL
import Control.Concurrent.STM.TMVar
import qualified Domain.Action.Search as DSearch
import qualified ExternalAPI.Flow as ExternalAPI
import GHC.Conc
import Kernel.Prelude
import Kernel.Streaming.Kafka.Consumer
import Kernel.Streaming.Kafka.Topic.PublicTransportSearch
import Kernel.Streaming.MonadConsumer
import Kernel.Types.Logging
import Kernel.Utils.Common
import Tools.Metrics

run ::
  ( HasField "isShuttingDown" r (TMVar ()),
    HasField "bapId" r Text,
    HasField "bapURI" r BaseUrl,
    HasField "gatewayUrl" r BaseUrl,
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    MonadConsumer PublicTransportSearch m,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  m ()
run = withLogTag "Service" $
  listenForMessages @PublicTransportSearch isRunning $ \searchReq -> withTransactionIdLogTag' searchReq.id $ do
    searchMessage <- DSearch.search searchReq
    becknSearchReq <- BecknACL.buildSearchReq searchMessage
    fork "search" . withShortRetry $ do
      -- do we need fork here?
      ExternalAPI.search becknSearchReq
  where
    isRunning = liftIO . atomically . isEmptyTMVar =<< asks (.isShuttingDown)
