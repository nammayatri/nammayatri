module API.RiderDashboard.Entity where

import qualified Domain.Action.RiderDashboard.Entity as DEntity
import qualified "lib-dashboard" Domain.Types.Entity as DE
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

handler :: BeamFlow' => FlowServer API
handler =
  listEntity
    :<|> createEntity
    :<|> updateEntity
    :<|> deleteEntity

listEntity :: BeamFlow' => TokenInfo -> Maybe Bool -> FlowHandler DEntity.ListEntityResp
listEntity _ mbIncludeDeleted =
  withFlowHandlerAPI' (DEntity.listEntity mbIncludeDeleted)

createEntity :: BeamFlow' => TokenInfo -> DEntity.CreateEntityReq -> FlowHandler DEntity.CreateEntityResp
createEntity tokenInfo req = withFlowHandlerAPI' (DEntity.createEntity tokenInfo.personId req)

updateEntity :: BeamFlow' => TokenInfo -> Id DE.Entity -> DEntity.UpdateEntityReq -> FlowHandler APISuccess
updateEntity tokenInfo entityId req = withFlowHandlerAPI' (DEntity.updateEntity tokenInfo.personId entityId req)

deleteEntity :: BeamFlow' => TokenInfo -> Id DE.Entity -> FlowHandler APISuccess
deleteEntity tokenInfo entityId = withFlowHandlerAPI' (DEntity.deleteEntity tokenInfo.personId entityId)
