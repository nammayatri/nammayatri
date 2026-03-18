module API.UI.RentalsIntercityCache
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.RentalsIntercityCache as RIC
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API =
  "fetchFareCache"
    :> TokenAuth
    :> ReqBody '[JSON] RIC.FareCacheReq
    :> Post '[JSON] RIC.FareCacheResp

handler :: FlowServer API
handler = rentalsIntercityCache

rentalsIntercityCache ::
  (Id Person.Person, Id Merchant.Merchant) ->
  RIC.FareCacheReq ->
  FlowHandler RIC.FareCacheResp
rentalsIntercityCache (personId, merchantId) req = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ RIC.rentalsIntercityCache personId merchantId req
