{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Vehicle
  ( API,
    handler,
    DVehicle.UpdateVehicleReq (..),
    DVehicle.GetVehicleRes (..),
    DVehicle.ListVehicleRes (..),
    DVehicle.UpdateVehicleRes,
    DVehicle.VehicleRes (..),
    DVehicle.Driver (..),
  )
where

import qualified Domain.Action.UI.Vehicle as DVehicle
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle.Variant as Variant
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

-- Following is vehicle flow
type API =
  "org" :> "vehicle"
    :> ( "list"
           :> AdminTokenAuth
           :> QueryParam "variant" Variant.Variant
           :> QueryParam "registrationNo" Text
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] DVehicle.ListVehicleRes
           :<|> AdminTokenAuth
             :> Capture "driverId" (Id SP.Person)
             :> ReqBody '[JSON] DVehicle.UpdateVehicleReq
             :> Post '[JSON] DVehicle.UpdateVehicleRes
           :<|> TokenAuth
             :> QueryParam "registrationNo" Text
             :> QueryParam "driverId" (Id SP.Person)
             :> Get '[JSON] DVehicle.GetVehicleRes
       )

handler :: FlowServer API
handler =
  listVehicles
    :<|> updateVehicle
    :<|> getVehicle

listVehicles :: SP.Person -> Maybe Variant.Variant -> Maybe Text -> Maybe Int -> Maybe Int -> FlowHandler DVehicle.ListVehicleRes
listVehicles admin variantM mbRegNum limitM = withFlowHandlerAPI . DVehicle.listVehicles admin variantM mbRegNum limitM

updateVehicle :: SP.Person -> Id SP.Person -> DVehicle.UpdateVehicleReq -> FlowHandler DVehicle.UpdateVehicleRes
updateVehicle admin driverId = withFlowHandlerAPI . DVehicle.updateVehicle admin driverId

getVehicle :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Text -> Maybe (Id SP.Person) -> FlowHandler DVehicle.GetVehicleRes
getVehicle (personId, merchantId, merchantOpCityId) registrationNoM = withFlowHandlerAPI . DVehicle.getVehicle (personId, merchantId, merchantOpCityId) registrationNoM
