{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Merchant where

import qualified Domain.Action.Dashboard.Merchant as DMerchant
import qualified Domain.Action.Dashboard.Person as DPerson
import Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.Common
import Servant
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)

type API =
  "admin"
    :> "merchant"
    :> ( "create"
           :> ( "withAdmin"
                  :> DashboardAuth 'DASHBOARD_ADMIN
                  :> ReqBody '[JSON] DMerchant.CreateMerchantWithAdminReq
                  :> Post '[JSON] DP.PersonAPIEntity
                  :<|> DashboardAuth 'DASHBOARD_ADMIN
                    :> ReqBody '[JSON] DMerchant.CreateMerchantReq
                    :> Post '[JSON] DMerchant.MerchantAPIEntity
              )
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> "list"
             :> QueryParam "limit" Int
             :> QueryParam "offset" Int
             :> QueryParam "shortId" Text
             :> Get '[JSON] DMerchant.ListMerchantResp
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> "change"
             :> "enableState"
             :> ReqBody '[JSON] DMerchant.ChangeMerchantEnableStateReq
             :> Post '[JSON] APISuccess
           :<|> DashboardAuth 'MERCHANT_ADMIN
             :> "create"
             :> "user"
             :> ReqBody '[JSON] DPerson.CreatePersonReq
             :> Post '[JSON] DPerson.CreatePersonRes
       )

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler =
  ( createMerchantWithAdmin
      :<|> createMerchant
  )
    :<|> listMerchants
    :<|> changeMerchantEnableState
    :<|> createUserForMerchant

createMerchantWithAdmin :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DMerchant.CreateMerchantWithAdminReq -> FlowHandlerR r DP.PersonAPIEntity
createMerchantWithAdmin tokenInfo =
  withDashboardDbFlowHandlerAPI . DMerchant.createMerchantWithAdmin tokenInfo

createMerchant :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DMerchant.CreateMerchantReq -> FlowHandlerR r DMerchant.MerchantAPIEntity
createMerchant tokenInfo =
  withDashboardDbFlowHandlerAPI . DMerchant.createMerchant tokenInfo

listMerchants :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> FlowHandlerR r DMerchant.ListMerchantResp
listMerchants tokenInfo mbLimit mbOffset mbShortId =
  withDashboardDbFlowHandlerAPI $ DMerchant.listMerchants tokenInfo mbLimit mbOffset mbShortId

changeMerchantEnableState :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DMerchant.ChangeMerchantEnableStateReq -> FlowHandlerR r APISuccess
changeMerchantEnableState tokenInfo req =
  withDashboardDbFlowHandlerAPI $ DMerchant.changeMerchantEnableState tokenInfo req

createUserForMerchant :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DPerson.CreatePersonReq -> FlowHandlerR r DPerson.CreatePersonRes
createUserForMerchant tokenInfo req =
  withDashboardDbFlowHandlerAPI $ DMerchant.createUserForMerchant tokenInfo req
