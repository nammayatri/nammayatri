module API.RiderDashboard.Entity where

import qualified Domain.Action.RiderDashboard.Entity as DEntity
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
    :> Capture "merchantShortId" (ShortId DMerchant.Merchant)
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

handler :: BeamFlow' => FlowServer API
handler merchantShortId =
  listEntity merchantShortId
    :<|> createEntity merchantShortId
    :<|> updateEntity merchantShortId
    :<|> deleteEntity merchantShortId

listEntity :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> Maybe Bool -> FlowHandler DEntity.ListEntityResp
listEntity merchantShortId _ mbIncludeDeleted =
  withFlowHandlerAPI' (DEntity.listEntity merchantShortId mbIncludeDeleted)

createEntity :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> DEntity.CreateEntityReq -> FlowHandler DEntity.CreateEntityResp
createEntity merchantShortId tokenInfo req = withFlowHandlerAPI' (DEntity.createEntity tokenInfo.personId merchantShortId req)

updateEntity :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> Id DE.Entity -> DEntity.UpdateEntityReq -> FlowHandler APISuccess
updateEntity merchantShortId tokenInfo entityId req = withFlowHandlerAPI' (DEntity.updateEntity tokenInfo.personId merchantShortId entityId req)

deleteEntity :: BeamFlow' => ShortId DMerchant.Merchant -> TokenInfo -> Id DE.Entity -> FlowHandler APISuccess
deleteEntity merchantShortId tokenInfo entityId = withFlowHandlerAPI' (DEntity.deleteEntity tokenInfo.personId merchantShortId entityId)
