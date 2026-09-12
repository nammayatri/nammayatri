{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Bulk creation of dashboard users from a CSV upload.
--
-- Like 'API.Dashboard.Entity', this sits under the V1 BAP path
-- (@\/bap\/{merchantId}\/person\/bulkCreate@), so the merchant capture is
-- reproduced and the path is unchanged.
module API.Dashboard.PersonBulk
  ( API,
    handler,
  )
where

import qualified Domain.Action.Dashboard.Person as DPerson
import qualified Domain.Types.Merchant as DMerchant
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Utils.Common (FlowHandlerR, FlowServerR)
import Servant
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)

type API =
  Capture "merchantId" (ShortId DMerchant.Merchant)
    :> "person"
    :> "bulkCreate"
    :> DashboardAuth 'DASHBOARD_USER
    :> ReqBody '[JSON] DPerson.BulkUpsertPersonReq
    :> Post '[JSON] DPerson.BulkUpsertPersonResp

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler = bulkCreate

-- Delegates to lib-dashboard's consolidated bulkUpsert (Domain.Action.Dashboard.Person),
-- which carries the capability gate. This module only mounts the route on the
-- direct-dashboard tree; the implementation is shared with provider-dashboard.
bulkCreate :: DashboardLoginFlow (FlowR r) r => ShortId DMerchant.Merchant -> TokenInfo -> DPerson.BulkUpsertPersonReq -> FlowHandlerR r DPerson.BulkUpsertPersonResp
bulkCreate merchantId tokenInfo req =
  withDashboardDbFlowHandlerAPI (DPerson.bulkUpsert tokenInfo merchantId req)
