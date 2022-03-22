module App.Routes.FarePolicy where

import App.Routes.FarePolicy.Discount
import App.Types
import Beckn.Types.Id (Id)
import Domain.Types.FarePolicy (FarePolicy)
import Product.FarePolicy (listFarePolicies, updateFarePolicy)
import Servant
import Types.API.FarePolicy
  ( ListFarePolicyRes,
    UpdateFarePolicyReq,
    UpdateFarePolicyRes,
  )
import Utils.Auth

type FarePolicyAPI =
  "org" :> "farePolicy"
    :> ( FPDiscountAPI
           :<|> AdminTokenAuth :> Get '[JSON] ListFarePolicyRes
           :<|> AdminTokenAuth
             :> Capture "farePolicyId" (Id FarePolicy)
             :> ReqBody '[JSON] UpdateFarePolicyReq
             :> Post '[JSON] UpdateFarePolicyRes
       )

farePolicyFlow :: FlowServer FarePolicyAPI
farePolicyFlow =
  discountFlow
    :<|> listFarePolicies
    :<|> updateFarePolicy
