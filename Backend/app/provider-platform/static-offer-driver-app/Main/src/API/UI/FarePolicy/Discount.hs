{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.FarePolicy.Discount (module Reexport, API, handler) where

import Domain.Action.UI.FarePolicy.Discount as Reexport
  ( CreateFarePolicyDiscountReq,
    CreateFarePolicyDiscountRes,
    DeleteFarePolicyDiscountRes,
    UpdateFarePolicyDiscountReq,
    UpdateFarePolicyDiscountRes,
  )
import qualified Domain.Action.UI.FarePolicy.Discount as DDiscount
import Domain.Types.FarePolicy.Discount (Discount)
import qualified Domain.Types.FarePolicy.Discount as DFPDiscount
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant
import Tools.Auth

type API =
  "discount"
    :> ( AdminTokenAuth
           :> ReqBody '[JSON] CreateFarePolicyDiscountReq
           :> Post '[JSON] CreateFarePolicyDiscountRes
           :<|> AdminTokenAuth
             :> Capture "discountId" (Id Discount)
             :> ReqBody '[JSON] UpdateFarePolicyDiscountReq
             :> Post '[JSON] UpdateFarePolicyDiscountRes
           :<|> AdminTokenAuth
             :> Capture "discountId" (Id Discount)
             :> Delete '[JSON] DeleteFarePolicyDiscountRes
       )

handler :: FlowServer API
handler =
  createFarePolicyDiscount
    :<|> updateFarePolicyDiscount
    :<|> deleteFarePolicyDiscount

createFarePolicyDiscount :: SP.Person -> CreateFarePolicyDiscountReq -> FlowHandler CreateFarePolicyDiscountRes
createFarePolicyDiscount admin = withFlowHandlerAPI . DDiscount.createFarePolicyDiscount admin

updateFarePolicyDiscount :: SP.Person -> Id DFPDiscount.Discount -> UpdateFarePolicyDiscountReq -> FlowHandler UpdateFarePolicyDiscountRes
updateFarePolicyDiscount admin discId = withFlowHandlerAPI . DDiscount.updateFarePolicyDiscount admin discId

deleteFarePolicyDiscount :: SP.Person -> Id DFPDiscount.Discount -> FlowHandler UpdateFarePolicyDiscountRes
deleteFarePolicyDiscount admin = withFlowHandlerAPI . DDiscount.deleteFarePolicyDiscount admin
