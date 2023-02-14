 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.FarePolicy.OneWayFarePolicy (module Reexport, API, handler) where

import Domain.Action.UI.FarePolicy.OneWayFarePolicy as Reexport
  ( ListOneWayFarePolicyRes (..),
    UpdateOneWayFarePolicyReq (..),
    UpdateOneWayFarePolicyRes,
  )
import qualified Domain.Action.UI.FarePolicy.OneWayFarePolicy as OneWayFP
import Domain.Types.FarePolicy.OneWayFarePolicy
import qualified Domain.Types.FarePolicy.OneWayFarePolicy as DFarePolicy
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  -- "oneWay" -- TODO: we should add this layer
  --   :> (
  AdminTokenAuth :> Get '[JSON] ListOneWayFarePolicyRes
    :<|> AdminTokenAuth
      :> Capture "farePolicyId" (Id OneWayFarePolicy)
      :> ReqBody '[JSON] UpdateOneWayFarePolicyReq
      :> Post '[JSON] UpdateOneWayFarePolicyRes

--  )

handler :: FlowServer API
handler =
  listOneWayFarePolicies
    :<|> updateOneWayFarePolicy

listOneWayFarePolicies :: SP.Person -> FlowHandler ListOneWayFarePolicyRes
listOneWayFarePolicies = withFlowHandlerAPI . OneWayFP.listOneWayFarePolicies

updateOneWayFarePolicy :: SP.Person -> Id DFarePolicy.OneWayFarePolicy -> UpdateOneWayFarePolicyReq -> FlowHandler UpdateOneWayFarePolicyRes
updateOneWayFarePolicy admin fpId = withFlowHandlerAPI . OneWayFP.updateOneWayFarePolicy admin fpId
