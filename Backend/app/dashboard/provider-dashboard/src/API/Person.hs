module API.Person where

import qualified Domain.Action.Person as DPerson
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import "lib-dashboard" Environment
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant
import "lib-dashboard" Storage.Beam.BeamFlow
import "lib-dashboard" Tools.Auth

-- DashboardAuth is coarse; fine-grained RBAC via verifyAccessLevel DASHBOARD_USER_BULK_CREATE inside the handler.
type API =
  "person"
    :> ( "bulkUpsert"
           :> DashboardAuth 'DASHBOARD_USER
           :> ReqBody '[JSON] DPerson.BulkUpsertPersonReq
           :> Post '[JSON] DPerson.BulkUpsertPersonResp
           -- TODO : Deprecated alias for bulkUpsert, remove once every CSV caller has moved.
           :<|> "bulkCreate"
             :> DashboardAuth 'DASHBOARD_USER
             :> ReqBody '[JSON] DPerson.BulkUpsertPersonReq
             :> Post '[JSON] DPerson.BulkUpsertPersonResp
       )

handler :: BeamFlow' => ShortId DMerchant.Merchant -> FlowServer API
handler merchantId = bulkUpsert merchantId :<|> bulkUpsert merchantId

bulkUpsert :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> DPerson.BulkUpsertPersonReq -> FlowHandler DPerson.BulkUpsertPersonResp
bulkUpsert merchantId tokenInfo req = withFlowHandlerAPI' (DPerson.bulkUpsert tokenInfo merchantId req)
