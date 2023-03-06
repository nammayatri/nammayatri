{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.FarePolicy
  ( API,
    handler,
    DFarePolicy.ListFarePolicyRes (..),
    DFarePolicy.UpdateFarePolicyReq (..),
    DFarePolicy.UpdateFarePolicyRes,
  )
where

import qualified Domain.Action.UI.FarePolicy as DFarePolicy
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "org"
    :> ( "farePolicy"
           :> ( AdminTokenAuth :> Get '[JSON] DFarePolicy.ListFarePolicyRes
                  :<|> AdminTokenAuth
                    :> Capture "farePolicyId" (Id DFP.FarePolicy)
                    :> ReqBody '[JSON] DFarePolicy.UpdateFarePolicyReq
                    :> Post '[JSON] DFarePolicy.UpdateFarePolicyRes
              )
       )

handler :: FlowServer API
handler =
  listFarePolicies
    :<|> updateFarePolicy

listFarePolicies :: SP.Person -> FlowHandler DFarePolicy.ListFarePolicyRes
listFarePolicies = withFlowHandlerAPI . DFarePolicy.listFarePolicies

updateFarePolicy :: SP.Person -> Id DFP.FarePolicy -> DFarePolicy.UpdateFarePolicyReq -> FlowHandler DFarePolicy.UpdateFarePolicyRes
updateFarePolicy admin fpId = withFlowHandlerAPI . DFarePolicy.updateFarePolicy admin fpId
