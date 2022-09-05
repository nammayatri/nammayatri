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

import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.Vehicle as DVehicle
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle.Variant as Variant
import Environment
import EulerHS.Prelude hiding (id)
import Servant
import Utils.Auth

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

getVehicle :: Id SP.Person -> Maybe Text -> Maybe (Id SP.Person) -> FlowHandler DVehicle.GetVehicleRes
getVehicle personId registrationNoM = withFlowHandlerAPI . DVehicle.getVehicle personId registrationNoM
