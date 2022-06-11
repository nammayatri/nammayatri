module App.Routes.FarePolicy where

import Beckn.Types.Id (Id)
import Domain.Types.FarePolicy (FarePolicy)
import Environment
import Product.FarePolicy
import Servant
import Types.API.FarePolicy
import Utils.Auth

-- total
type FarePolicyAPI =
  "org"
    :> ( "farePolicy"
           :> ( AdminTokenAuth :> Get '[JSON] ListFarePolicyRes
                  :<|> AdminTokenAuth
                    :> Capture "farePolicyId" (Id FarePolicy)
                    :> ReqBody '[JSON] UpdateFarePolicyReq
                    :> Post '[JSON] UpdateFarePolicyRes
              )
       )

farePolicyFlow :: FlowServer FarePolicyAPI
farePolicyFlow =
  listFarePolicies
    :<|> updateFarePolicy
