{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Service.Runner where

import qualified Beckn.ACL.Search as BecknACL
import Control.Concurrent.STM.TMVar
import qualified Data.HashMap.Strict as HM
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
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    EsqDBFlow m r
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
