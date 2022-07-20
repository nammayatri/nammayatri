module API.UI.FarePolicy.RentalFarePolicy.Handler (API, handler) where

import API.UI.FarePolicy.RentalFarePolicy.Types
import App.Types
import Beckn.Types.APISuccess
import qualified Domain.Action.UI.FarePolicy.RentalFarePolicy as RentalFP
import qualified Domain.Types.Person as SP
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common

type API =
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

handler :: FlowServer API
handler p =
  listRentalFarePolicies p
    :<|> createRentalFarePolicy p

createRentalFarePolicy :: SP.Person -> CreateRentalFarePolicyReq -> FlowHandler APISuccess
createRentalFarePolicy admin = withFlowHandlerAPI . RentalFP.createRentalFarePolicy admin

listRentalFarePolicies :: SP.Person -> FlowHandler ListRentalFarePoliciesRes
listRentalFarePolicies = withFlowHandlerAPI . RentalFP.listRentalFarePolicies
