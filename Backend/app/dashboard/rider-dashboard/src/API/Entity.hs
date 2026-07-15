module API.Entity where

import qualified Domain.Action.Entity as DEntity
import qualified "lib-dashboard" Domain.Types.Entity as DE
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant
import "lib-dashboard" Storage.Beam.BeamFlow
import "lib-dashboard" Tools.Auth

type API =
  "entity"
    :> ( DashboardAuth 'DASHBOARD_USER
           :> "list"
           :> QueryParam "includeDeleted" Bool
           :> Get '[JSON] DEntity.ListEntityResp
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> "create"
             :> ReqBody '[JSON] DEntity.CreateEntityReq
             :> Post '[JSON] DEntity.CreateEntityResp
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "entityId" (Id DE.Entity)
             :> "update"
             :> ReqBody '[JSON] DEntity.UpdateEntityReq
             :> Post '[JSON] APISuccess
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "entityId" (Id DE.Entity)
             :> Delete '[JSON] APISuccess
       )

handler :: BeamFlow' => ShortId DMerchant.Merchant -> FlowServer API
handler merchantId =
  listEntity merchantId
    :<|> createEntity merchantId
    :<|> updateEntity merchantId
    :<|> deleteEntity merchantId

listEntity :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> Maybe Bool -> FlowHandler DEntity.ListEntityResp
listEntity merchantId _ mbIncludeDeleted =
  withFlowHandlerAPI' (DEntity.listEntity merchantId mbIncludeDeleted)

createEntity :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> DEntity.CreateEntityReq -> FlowHandler DEntity.CreateEntityResp
createEntity merchantId tokenInfo req = withFlowHandlerAPI' (DEntity.createEntity tokenInfo.personId merchantId req)

updateEntity :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> Id DE.Entity -> DEntity.UpdateEntityReq -> FlowHandler APISuccess
updateEntity merchantId tokenInfo entityId req = withFlowHandlerAPI' (DEntity.updateEntity tokenInfo.personId merchantId entityId req)

deleteEntity :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> Id DE.Entity -> FlowHandler APISuccess
deleteEntity merchantId tokenInfo entityId = withFlowHandlerAPI' (DEntity.deleteEntity tokenInfo.personId merchantId entityId)
