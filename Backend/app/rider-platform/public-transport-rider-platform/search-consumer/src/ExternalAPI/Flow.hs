{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ExternalAPI.Flow where

import Beckn.Spec.API.Search as Search
import qualified Beckn.Spec.Search as Search
import qualified Data.HashMap.Strict as HM
import qualified EulerHS.Types as ET
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.App
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.SignatureAuth

search ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasField "bapId" r Text,
    HasField "gatewayUrl" r BaseUrl,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  BecknReq Search.SearchMessage ->
  m ()
search req = do
  url <- asks (.gatewayUrl)
  callBecknAPIWithSignature "search" Search.searchAPI url req

callBecknAPIWithSignature ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api,
    HasField "bapId" r Text,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m ()
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.bapId)
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  void $ callBecknAPI (Just $ ET.ManagerSelector $ getHttpManagerKey bapId) Nothing a b c internalEndPointHashMap d
