{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Merchant-scoped dashboard-user administration: bulk CSV upsert and the PT
-- staff listing.
--
-- Like 'API.Dashboard.Entity', this sits under the V1 BAP path
-- (@\/bap\/{merchantId}\/person\/...@), so the merchant capture is reproduced
-- and the paths are unchanged.
module API.Dashboard.PersonBulk
  ( API,
    handler,
  )
where

import qualified Domain.Action.Dashboard.Person as DPerson
import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (FlowHandlerR, FlowServerR)
import Servant
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)

type API =
  Capture "merchantId" (ShortId DMerchant.Merchant)
    :> "person"
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

handler :: DashboardLoginFlow r => FlowServerR r API
handler merchantId = bulkUpsert merchantId :<|> bulkUpsert merchantId :<|> listPerson merchantId

-- Both route names share one handler: the upsert semantics were always what the
-- action did, so the deprecated bulkCreate path keeps working unchanged.
bulkUpsert :: DashboardLoginFlow r => ShortId DMerchant.Merchant -> TokenInfo -> DPerson.BulkUpsertPersonReq -> FlowHandlerR r DPerson.BulkUpsertPersonResp
bulkUpsert merchantId tokenInfo req =
  withDashboardDbFlowHandlerAPI (DPerson.bulkUpsert tokenInfo merchantId req)

listPerson :: DashboardLoginFlow r => ShortId DMerchant.Merchant -> TokenInfo -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandlerR r DPerson.ListPTEmployeeRes
listPerson merchantId tokenInfo mbSearchString mbRoleName mbEntityShortId mbTokenNo mbLimit =
  withDashboardDbFlowHandlerAPI . DPerson.ptList tokenInfo merchantId mbSearchString mbRoleName mbEntityShortId mbTokenNo mbLimit
