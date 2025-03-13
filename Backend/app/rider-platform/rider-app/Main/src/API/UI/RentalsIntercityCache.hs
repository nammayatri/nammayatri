module API.UI.RentalsIntercityCache
  ( API,
    handler,
  )
where

-- import qualified Domain.Action.UI.RentalsIntercityCache as 
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import qualified Domain.Action.UI.RentalsIntercityCache as RIC

type API =
  "rentalsIntercityCache"
    :> TokenAuth
    :> ReqBody '[JSON] RIC.RentalsIntercityCacheReq
    :> Post '[JSON] RIC.RentalsIntercityCacheResp

handler :: FlowServer API
handler = rentalsIntercityCache

rentalsIntercityCache :: 
  (Id Person.Person, Id Merchant.Merchant) ->
  RIC.RentalsIntercityCacheReq ->
  FlowHandler RIC.RentalsIntercityCacheResp
rentalsIntercityCache (personId, merchantId) req = withFlowHandlerAPI $ RIC.rentalsIntercityCache personId merchantId req
