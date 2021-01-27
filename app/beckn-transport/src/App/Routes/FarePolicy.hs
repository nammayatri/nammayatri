module App.Routes.FarePolicy where

import App.Types (FlowServer)
import Beckn.Types.ID (ID)
import Product.FarePolicy (listFarePolicies, updateFarePolicy)
import Servant
import Types.API.FarePolicy
  ( ListFarePolicyResponse,
    UpdateFarePolicyRequest,
  )
import Types.Domain.FarePolicy (FarePolicy)
import Utils.Common (AdminTokenAuth)

type FarePolicyAPI =
  "farePolicy"
    :> ( AdminTokenAuth :> Get '[JSON] ListFarePolicyResponse
           :<|> AdminTokenAuth
             :> Capture "farePolicyId" (ID FarePolicy)
             :> ReqBody '[JSON] UpdateFarePolicyRequest
             :> Post '[JSON] () -- TODO Ack?
       )

farePolicyFlow :: FlowServer FarePolicyAPI
farePolicyFlow = listFarePolicies :<|> updateFarePolicy
