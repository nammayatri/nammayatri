module API.BPP.DriverOffer.Driver
  ( API,
    handler,
  )
where

import qualified "driver-offer-bpp" API.Dashboard.Driver as DriverOfferBpp
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import "lib-dashboard" Domain.Types.Person as DP
import "lib-dashboard" Environment
import qualified EulerHS.Types as T
import Servant
import "lib-dashboard" Tools.Auth
import qualified Tools.Client as Client

type API =
  "driver"
    :> "list"
    :> ApiAuth 'READ_ACCESS 'DRIVERS
    :> Get '[JSON] Text

handler :: FlowServer API
handler =
  listDriver

listDriver :: Id DP.Person -> FlowHandler Text
listDriver _ = withFlowHandlerAPI $ do
  Client.callDriverOfferApi client "driverOfferBppDriverList"
  where
    driverOfferBppDriverListAPI :: Proxy DriverOfferBpp.DriverListAPI
    driverOfferBppDriverListAPI = Proxy
    client =
      T.client
        driverOfferBppDriverListAPI
