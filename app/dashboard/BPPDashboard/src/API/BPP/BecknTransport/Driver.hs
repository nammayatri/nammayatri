module API.BPP.BecknTransport.Driver where

import qualified "beckn-transport" API.Dashboard.Driver as BecknTransport
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import "lib-dashboard" Domain.Types.Person as DP
import "lib-dashboard" Environment
import qualified EulerHS.Types as T
import Servant
import "lib-dashboard" Tools.Auth
import Tools.Client

type API =
  "driver"
    :> "list"
    :> TokenAuth (ApiAccessLevel 'READ_ACCESS 'DRIVERS)
    :> Get '[JSON] Text

handler :: FlowServer API
handler =
  listDriver

listDriver :: Id DP.Person -> FlowHandler Text
listDriver _ = withFlowHandlerAPI $ do
  callDriverOfferApi client "becknTransportDriverList"
  where
    becknTransportDriverListAPI :: Proxy BecknTransport.DriverListAPI
    becknTransportDriverListAPI = Proxy
    client =
      T.client
        becknTransportDriverListAPI
