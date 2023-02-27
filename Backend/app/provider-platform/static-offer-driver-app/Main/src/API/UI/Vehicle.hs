{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Vehicle (module Reexport, API, handler) where

import Domain.Action.UI.Vehicle as Reexport
  ( Driver (..),
    GetVehicleRes (..),
    ListVehicleRes (..),
    UpdateVehicleReq (..),
    UpdateVehicleRes,
    VehicleRes (..),
  )
import qualified Domain.Action.UI.Vehicle as DVeh
import Domain.Types.Person as SP
import Domain.Types.Vehicle as SV
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth (AdminTokenAuth, TokenAuth)

type API =
  "org" :> "vehicle"
    :> ( "list"
           :> AdminTokenAuth
           :> QueryParam "variant" Variant
           :> QueryParam "registrationNo" Text
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] ListVehicleRes
           :<|> AdminTokenAuth
             :> Capture "driverId" (Id Person)
             :> ReqBody '[JSON] UpdateVehicleReq
             :> Post '[JSON] UpdateVehicleRes
           :<|> TokenAuth
             :> QueryParam "registrationNo" Text
             :> QueryParam "driverId" (Id Person)
             :> Get '[JSON] GetVehicleRes
       )

handler :: FlowServer API
handler =
  listVehicles
    :<|> updateVehicle
    :<|> getVehicle

listVehicles :: SP.Person -> Maybe SV.Variant -> Maybe Text -> Maybe Int -> Maybe Int -> FlowHandler ListVehicleRes
listVehicles admin variantM mbRegNum limitM = withFlowHandlerAPI . DVeh.listVehicles admin variantM mbRegNum limitM

updateVehicle :: SP.Person -> Id SP.Person -> UpdateVehicleReq -> FlowHandler UpdateVehicleRes
updateVehicle admin driverId = withFlowHandlerAPI . DVeh.updateVehicle admin driverId

getVehicle :: Id SP.Person -> Maybe Text -> Maybe (Id SP.Person) -> FlowHandler GetVehicleRes
getVehicle personId registrationNoM = withFlowHandlerAPI . DVeh.getVehicle personId registrationNoM
