module App.Routes.FarePolicy.Rentals where

import App.Types
import Beckn.Types.APISuccess (APISuccess)
import Product.FarePolicy.RentalFarePolicy
import Servant
import Types.API.FarePolicy.Rentals
import Utils.Auth

-- rentals fare policies
type FPRentalsAPI =
  "rentals"
    :> AdminTokenAuth
    :> ( ListRentalFarePoliciesAPI
           :<|> CreateRentalFarePoliciesAPI
       )

type ListRentalFarePoliciesAPI =
  Get '[JSON] ListRentalFarePoliciesRes

type CreateRentalFarePoliciesAPI =
  ReqBody '[JSON] CreateRentalFarePolicyReq
    :> Post '[JSON] APISuccess

fpRentalsFlow :: FlowServer FPRentalsAPI
fpRentalsFlow p =
  listRentalFarePolicies p
    :<|> createRentalFarePolicy p
