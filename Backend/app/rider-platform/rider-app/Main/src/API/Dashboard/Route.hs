{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Route where

import Domain.Action.Dashboard.Route (TripRouteReq, mkGetLocation)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DP
import Environment
import qualified Kernel.External.Maps as Maps
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Prelude

data RouteEndPoint = TripRouteAPIEndPoint
  deriving (Show, Read)

type API =
  "ride"
    :> TripRouteAPI

type TripRouteAPI =
  "trip"
    :> "route"
    :> Capture "rideId" (Id DP.Ride)
    :> ReqBody '[JSON] TripRouteReq
    :> Post '[JSON] Maps.GetRoutesResp

handler :: ShortId DM.Merchant -> FlowServer API
handler =
  callgetTripRoute

callgetTripRoute :: ShortId DM.Merchant -> Id DP.Ride -> TripRouteReq -> FlowHandler Maps.GetRoutesResp
callgetTripRoute merchantShortId rideId req = withFlowHandlerAPI $ mkGetLocation merchantShortId rideId req
