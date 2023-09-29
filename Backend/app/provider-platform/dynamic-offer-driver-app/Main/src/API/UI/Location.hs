{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- module API.UI.Location
--   ( API,
--     handler,
--     DLocation.GetLocationRes,
--     DLocation.Waypoint (..),
--     DLocation.UpdateLocationReq,
--   )
-- where

-- import qualified Domain.Action.UI.Location as DLocation
-- import qualified Domain.Action.UI.Location.UpdateLocation as DLocation
-- import qualified Domain.Types.Merchant as DM
-- import qualified Domain.Types.Person as Person
-- import qualified Domain.Types.Ride as SRide
-- import Environment
-- import Kernel.Prelude
-- import Kernel.Types.APISuccess (APISuccess (..))
-- import Kernel.Types.Id
-- import Kernel.Utils.Common hiding (id)
-- import Servant
-- import Tools.Auth

-- -- Location update and get for tracking is as follows
-- type API =
--   "driver" :> "location"
--     :> ( Capture "rideId" (Id SRide.Ride)
--            :> Get '[JSON] DLocation.GetLocationRes
--            :<|> TokenAuth
--              :> ReqBody '[JSON] DLocation.UpdateLocationReq
--              :> Post '[JSON] APISuccess
--        )

-- handler :: FlowServer API
-- handler =
--   getLocation
--     :<|> updateLocation

-- updateLocation :: (Id Person.Person, Id DM.Merchant) -> DLocation.UpdateLocationReq -> FlowHandler APISuccess
-- updateLocation (personId, _) waypoints = withFlowHandlerAPI $ do
--   hdl <- DLocation.buildUpdateLocationHandle personId
--   DLocation.updateLocationHandler hdl waypoints

-- getLocation :: Id SRide.Ride -> FlowHandler DLocation.GetLocationRes
-- getLocation = withFlowHandlerAPI . DLocation.getLocation
