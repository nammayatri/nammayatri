module API.Person where

import qualified "lib-dashboard" Domain.Action.Dashboard.Person as DPerson
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import "lib-dashboard" Environment
import Kernel.Prelude
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
           :<|> "list"
             :> DashboardAuth 'DASHBOARD_USER
             :> QueryParam "searchString" Text
             :> QueryParam "roleName" Text
             :> QueryParam "entityShortId" Text
             :> QueryParam "tokenNo" Text
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] DPerson.ListPTEmployeeRes
       )

handler :: BeamFlow' => ShortId DMerchant.Merchant -> FlowServer API
handler merchantId = bulkUpsert merchantId :<|> bulkUpsert merchantId :<|> listPerson merchantId

bulkUpsert :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> DPerson.BulkUpsertPersonReq -> FlowHandler DPerson.BulkUpsertPersonResp
bulkUpsert merchantId tokenInfo req = withFlowHandlerAPI' (DPerson.bulkUpsert tokenInfo merchantId req)

listPerson :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DPerson.ListPTEmployeeRes
listPerson merchantId tokenInfo mbSearchString mbRoleName mbEntityShortId mbTokenNo mbLimit =
  withFlowHandlerAPI' . DPerson.ptList tokenInfo merchantId mbSearchString mbRoleName mbEntityShortId mbTokenNo mbLimit
