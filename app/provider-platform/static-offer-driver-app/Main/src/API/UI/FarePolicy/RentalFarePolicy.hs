module API.UI.FarePolicy.RentalFarePolicy (module Reexport, API, handler) where

import Domain.Action.UI.FarePolicy.RentalFarePolicy as Reexport
  ( CreateRentalFarePolicyItem (..),
    CreateRentalFarePolicyReq (..),
    ListRentalFarePoliciesRes (..),
  )
import qualified Domain.Action.UI.FarePolicy.RentalFarePolicy as RentalFP
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Tools.Auth

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
