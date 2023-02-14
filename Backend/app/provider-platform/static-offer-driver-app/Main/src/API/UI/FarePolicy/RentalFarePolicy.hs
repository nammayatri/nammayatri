 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
