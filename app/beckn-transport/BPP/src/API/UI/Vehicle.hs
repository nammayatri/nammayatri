module API.UI.Vehicle (module Reexport, API, handler) where

import Beckn.Types.Id
import Beckn.Utils.Common
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
