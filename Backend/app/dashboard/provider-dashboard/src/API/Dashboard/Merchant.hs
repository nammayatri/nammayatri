{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Merchant where

import "lib-dashboard" API.Dashboard.Merchant
import qualified "lib-dashboard" Domain.Action.Dashboard.Merchant as DMerchant
import qualified "lib-dashboard" Domain.Action.Dashboard.Person as DPerson
import "lib-dashboard" Domain.Types.Merchant as DMerchant
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth

-- Note : Type of API is defined in lib-dashboard/API/Dashboard/Merchant.hs

handler :: FlowServer API
handler =
  createMerchant
    :<|> listMerchants
    :<|> createUserForMerchant

createMerchant :: TokenInfo -> DMerchant.CreateMerchantReq -> FlowHandler DMerchant.MerchantAPIEntity
createMerchant tokenInfo =
  withFlowHandlerAPI' . DMerchant.createMerchant tokenInfo

listMerchants :: TokenInfo -> FlowHandler [DMerchant.MerchantAPIEntity]
listMerchants tokenInfo =
  withFlowHandlerAPI' $ DMerchant.listMerchants tokenInfo

createUserForMerchant :: TokenInfo -> DPerson.CreatePersonReq -> FlowHandler DPerson.CreatePersonRes
createUserForMerchant tokenInfo req =
  withFlowHandlerAPI' $ DMerchant.createUserForMerchant tokenInfo req
