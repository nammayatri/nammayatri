module API.UI.FarePolicy.Discount (module Reexport, API, handler) where

import App.Types
import Beckn.Types.Id (Id (..))
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
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common (withFlowHandlerAPI)

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
