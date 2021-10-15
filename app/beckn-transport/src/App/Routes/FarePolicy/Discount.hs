module App.Routes.FarePolicy.Discount where

import App.Types
import Beckn.Types.Id (Id)
import Product.FarePolicy.Discount
import Servant
import Types.API.FarePolicy.Discount
import Types.Domain.FarePolicy.Discount (Discount)
import Utils.Auth

type FPDiscountAPI =
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
             :> Post '[JSON] DeleteFarePolicyDiscountRes
       )

discountFlow :: FlowServer FPDiscountAPI
discountFlow =
  createFarePolicyDiscount
    :<|> updateFarePolicyDiscount
    :<|> deleteFarePolicyDiscount
