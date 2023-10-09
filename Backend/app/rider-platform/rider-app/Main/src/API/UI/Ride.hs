{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Ride
  ( API,
    handler,
    DRide.GetDriverLocResp,
    DRide.GetRideStatusResp (..),
    DRide.EditLocationReq (..),
  )
where

import Data.Aeson.Types ()
import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as SPerson
import qualified Domain.Types.Ride as SRide
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "ride"
    :> ( Capture "rideId" (Id SRide.Ride)
           :> ( "driver"
                  :> "location"
                  :> TokenAuth
                  :> Post '[JSON] DRide.GetDriverLocResp
                  :<|> "status"
                  :> TokenAuth
                  :> Get '[JSON] DRide.GetRideStatusResp
                  :<|> "edit"
                  :> "location"
                  :> TokenAuth
                  :> ReqBody '[JSON] DRide.EditLocationReq
                  :> Post '[JSON] APISuccess
              )
       )

handler :: FlowServer API
handler rideId =
  getDriverLoc rideId
    :<|> getRideStatus rideId
    :<|> editLocation rideId

getDriverLoc :: Id SRide.Ride -> (Id SPerson.Person, Id Merchant.Merchant) -> FlowHandler DRide.GetDriverLocResp
getDriverLoc rideId (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId $ DRide.getDriverLoc rideId personId

getRideStatus :: Id SRide.Ride -> (Id SPerson.Person, Id Merchant.Merchant) -> FlowHandler DRide.GetRideStatusResp
getRideStatus rideId (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId $ DRide.getRideStatus rideId personId

editLocation :: Id SRide.Ride -> (Id SPerson.Person, Id Merchant.Merchant) -> DRide.EditLocationReq -> FlowHandler APISuccess
editLocation rideId (personId, merchantId) editLocationReq = withFlowHandlerAPI . withPersonIdLogTag personId $ DRide.editLocation rideId (personId, merchantId) editLocationReq
