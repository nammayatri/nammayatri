module API.Handler where

import qualified API.Types as API
import qualified Data.ByteString as BS
import Environment
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
trigger urlText body = withFlowHandlerBecknAPI $ do
  url <- parseBaseUrl urlText
  logInfo $ decodeUtf8 body
  callBAP url body

callBAP ::
  ( MonadFlow m,
    HasFlowEnv m r '["selfId" ::: Text],
    CoreMetrics m
  ) =>
  BaseUrl ->
  BS.ByteString ->
  m AckResponse
callBAP uri body = do
  selfId <- asks (.selfId)
  let authKey = getHttpManagerKey selfId
  Beckn.callBecknAPI (Just authKey) Nothing "Some action" fakeAPI uri body
  where
    fakeAPI :: Proxy (ReqBody '[JSONBS] BS.ByteString :> Post '[JSON] AckResponse)
    fakeAPI = Proxy

callbackReceiver :: SignatureAuthResult -> Text -> BS.ByteString -> FlowHandler AckResponse
callbackReceiver _ action body = withFlowHandlerBecknAPI $ do
  logInfo $ "Received " <> action <> " callback with body: " <> decodeUtf8 body
  return Ack
