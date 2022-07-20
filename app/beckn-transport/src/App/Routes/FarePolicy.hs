module App.Routes.FarePolicy where

import App.Routes.FarePolicy.Discount
import App.Routes.FarePolicy.FareProduct
import App.Routes.FarePolicy.Rentals
import App.Types
import Beckn.Types.Id (Id)
import Domain.Types.FarePolicy.OneWayFarePolicy (FarePolicy)
import Product.FarePolicy.OneWayFarePolicy
import Servant
import Types.API.FarePolicy
import Utils.Auth

-- total
type FarePolicyAPI =
  "org"
    :> ( FareProductAPI
           :<|> "farePolicy"
           :> ( FPDiscountAPI
                  :<|> FPRentalsAPI
                  :<|> AdminTokenAuth :> Get '[JSON] ListFarePolicyRes
                  :<|> AdminTokenAuth
                    :> Capture "farePolicyId" (Id FarePolicy)
                    :> ReqBody '[JSON] UpdateFarePolicyReq
                    :> Post '[JSON] UpdateFarePolicyRes
              )
       )

farePolicyFlow :: FlowServer FarePolicyAPI
farePolicyFlow =
  fareProductFlow
    :<|> ( discountFlow
             :<|> fpRentalsFlow
             :<|> listOneWayFarePolicies
             :<|> updateOneWayFarePolicy
         )
