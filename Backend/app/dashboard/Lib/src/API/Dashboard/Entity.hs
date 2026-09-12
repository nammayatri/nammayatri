{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Entity administration (public-transport employers).
--
-- Dashboard-domain data -- the tables live in @atlas_dashboard@ and nothing here
-- touches an application package -- so it belongs with the rest of the
-- administration tree on driver-app.
--
-- The path carries a merchant but no city: provider-dashboard mounts this under
-- the V1 BAP tree (@\/bap\/{merchantId}\/entity\/...@), which has only the one
-- capture. The capture is reproduced here so the path is unchanged.
module API.Dashboard.Entity
  ( API,
    handler,
  )
where

import qualified Domain.Action.Dashboard.Entity as DEntity
import qualified Domain.Types.Entity as DE
import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Utils.Common (FlowHandlerR, FlowServerR)
import Servant
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)

type API =
  Capture "merchantId" (ShortId DMerchant.Merchant)
    :> "entity"
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

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler merchantId =
  listEntity merchantId
    :<|> createEntity merchantId
    :<|> updateEntity merchantId
    :<|> deleteEntity merchantId

listEntity :: DashboardLoginFlow (FlowR r) r => ShortId DMerchant.Merchant -> TokenInfo -> Maybe Bool -> FlowHandlerR r DEntity.ListEntityResp
listEntity merchantId _ mbIncludeDeleted =
  withDashboardDbFlowHandlerAPI (DEntity.listEntity merchantId mbIncludeDeleted)

createEntity :: DashboardLoginFlow (FlowR r) r => ShortId DMerchant.Merchant -> TokenInfo -> DEntity.CreateEntityReq -> FlowHandlerR r DEntity.CreateEntityResp
createEntity merchantId tokenInfo req =
  withDashboardDbFlowHandlerAPI (DEntity.createEntity tokenInfo.personId merchantId req)

updateEntity :: DashboardLoginFlow (FlowR r) r => ShortId DMerchant.Merchant -> TokenInfo -> Id DE.Entity -> DEntity.UpdateEntityReq -> FlowHandlerR r APISuccess
updateEntity merchantId tokenInfo entityId req =
  withDashboardDbFlowHandlerAPI (DEntity.updateEntity tokenInfo.personId merchantId entityId req)

deleteEntity :: DashboardLoginFlow (FlowR r) r => ShortId DMerchant.Merchant -> TokenInfo -> Id DE.Entity -> FlowHandlerR r APISuccess
deleteEntity merchantId tokenInfo entityId =
  withDashboardDbFlowHandlerAPI (DEntity.deleteEntity tokenInfo.personId merchantId entityId)
