{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Handler where

import qualified API.Types as API
import qualified Data.ByteString as BS
import qualified Data.HashMap as HM
import Environment
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.App
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Common
import qualified Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Kernel.Utils.Servant.JSONBS
import Kernel.Utils.Servant.SignatureAuth
import Servant

handler :: FlowServer API.API
handler = trigger :<|> callbackReceiver

trigger :: Text -> BS.ByteString -> FlowHandler AckResponse
trigger urlText body = withFlowHandlerBecknAPI' $ do
  url <- parseBaseUrl urlText
  logInfo $ decodeUtf8 body
  callBAP url body

callBAP ::
  ( MonadFlow m,
    HasFlowEnv m r '["selfId" ::: Text],
    HasField "aclEndPointHashMap" r (HM.Map Text Text),
    CoreMetrics m
  ) =>
  BaseUrl ->
  BS.ByteString ->
  m AckResponse
callBAP uri body = do
  selfId <- asks (.selfId)
  let authKey = getHttpManagerKey selfId
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing "Some action" fakeAPI uri aclEndPointHashMap body
  where
    fakeAPI :: Proxy (ReqBody '[JSONBS] BS.ByteString :> Post '[JSON] AckResponse)
    fakeAPI = Proxy

callbackReceiver :: SignatureAuthResult -> Text -> BS.ByteString -> FlowHandler AckResponse
callbackReceiver _ action body = withFlowHandlerBecknAPI' $ do
  logInfo $ "Received " <> action <> " callback with body: " <> decodeUtf8 body
  return Ack
