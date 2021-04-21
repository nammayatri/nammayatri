module App.Routes.FarePolicy where

import App.Types
import Beckn.Types.Id (Id)
import Product.FarePolicy (listFarePolicies, updateFarePolicy)
import Servant
import Types.API.FarePolicy
  ( ListFarePolicyResponse,
    UpdateFarePolicyRequest,
    UpdateFarePolicyResponse,
  )
import Types.Domain.FarePolicy (FarePolicy)
import Utils.Auth

type FarePolicyAPI =
  "farePolicy"
    :> ( TokenAuth :> Get '[JSON] ListFarePolicyResponse
           :<|> TokenAuth
             :> Capture "farePolicyId" (Id FarePolicy)
             :> ReqBody '[JSON] UpdateFarePolicyRequest
             :> Post '[JSON] UpdateFarePolicyResponse
       )

farePolicyFlow :: FlowServer FarePolicyAPI
farePolicyFlow =
  listFarePolicies
    :<|> updateFarePolicy
