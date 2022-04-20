module App.Routes.FarePolicy.FareProduct where

import App.Types
import Beckn.Types.APISuccess (APISuccess)
import Product.FarePolicy.FareProduct
import Servant
import Types.API.FarePolicy.FareProduct
import Utils.Auth

type FareProductAPI =
  "fareProduct"
    :> ( AdminTokenAuth
           :> Get '[JSON] ListFareProductsRes
           :<|> AdminTokenAuth
             :> ReqBody '[JSON] UpdateFareProductReq
             :> Post '[JSON] APISuccess
       )

fareProductFlow :: FlowServer FareProductAPI
fareProductFlow =
  listFareProducts
    :<|> updateFareProduct
