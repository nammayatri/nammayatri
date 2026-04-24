module API.Internal.InMemManagement where

import Environment (FlowHandler, FlowServer)
import Kernel.Prelude
import Kernel.Storage.InMem.Management.API (InMemManagementAPI)
import qualified Kernel.Storage.InMem.Management.Handler as Handler
import Kernel.Storage.InMem.Management.Types
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant

type API = InMemManagementAPI

handler :: FlowServer API
handler mbToken =
  keysHandler mbToken
    :<|> getHandler mbToken
    :<|> refreshHandler mbToken
    :<|> serverInfoHandler mbToken

keysHandler :: Maybe Text -> FlowHandler InMemKeysResponse
keysHandler = withFlowHandlerAPI . Handler.getKeys

getHandler :: Maybe Text -> InMemGetRequest -> FlowHandler InMemGetResponse
getHandler mbToken req = withFlowHandlerAPI $ Handler.getValue mbToken req

refreshHandler :: Maybe Text -> InMemRefreshRequest -> FlowHandler InMemRefreshResponse
refreshHandler mbToken req = withFlowHandlerAPI $ Handler.refreshCache mbToken req

serverInfoHandler :: Maybe Text -> FlowHandler InMemServerInfoResponse
serverInfoHandler mbToken = withFlowHandlerAPI $ Handler.getServerInfo mbToken "driver-app"
