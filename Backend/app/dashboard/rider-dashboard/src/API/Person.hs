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
    :> "bulkCreate"
    :> DashboardAuth 'DASHBOARD_USER
    :> ReqBody '[JSON] DPerson.BulkCreatePersonReq
    :> Post '[JSON] DPerson.BulkCreatePersonResp

handler :: BeamFlow' => ShortId DMerchant.Merchant -> FlowServer API
handler merchantId = bulkCreate merchantId

bulkCreate :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> DPerson.BulkCreatePersonReq -> FlowHandler DPerson.BulkCreatePersonResp
bulkCreate merchantId tokenInfo req = withFlowHandlerAPI' (DPerson.bulkCreate tokenInfo merchantId req)
