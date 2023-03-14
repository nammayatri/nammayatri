{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Maps
  ( API,
    handler,
    DMaps.AutoCompleteReq,
    DMaps.AutoCompleteResp,
    DMaps.GetPlaceDetailsReq,
    DMaps.GetPlaceDetailsResp,
    DMaps.GetPlaceNameReq,
    DMaps.GetPlaceNameResp,
  )
where

import qualified Domain.Action.UI.Maps as DMaps
import qualified Domain.Types.Person as Person
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Kernel.Utils.Logging
import Servant
import Tools.Auth

type API =
  "maps"
    :> ( "autoComplete"
           :> TokenAuth
           :> ReqBody '[JSON] DMaps.AutoCompleteReq
           :> Post '[JSON] DMaps.AutoCompleteResp
           :<|> "getPlaceDetails"
             :> TokenAuth
             :> ReqBody '[JSON] DMaps.GetPlaceDetailsReq
             :> Post '[JSON] DMaps.GetPlaceDetailsResp
           :<|> "getPlaceName"
             :> TokenAuth
             :> ReqBody '[JSON] DMaps.GetPlaceNameReq
             :> Post '[JSON] DMaps.GetPlaceNameResp
           :<|> "getDistance"
             :> TokenAuth
             :> ReqBody '[JSON] DMaps.GetDistanceReq
             :> Post '[JSON] DMaps.GetDistanceResp
           :<|> "getRoutes"
             :> TokenAuth
             :> ReqBody '[JSON] DMaps.GetRoutesReq
             :> Post '[JSON] DMaps.GetRoutesResp
           :<|> "snapToRoad"
             :> TokenAuth
             :> ReqBody '[JSON] DMaps.SnapToRoadReq
             :> Post '[JSON] DMaps.SnapToRoadResp
       )

handler :: FlowServer API
handler =
  autoComplete
    :<|> getPlaceDetails
    :<|> getPlaceName
    :<|> getDistance
    :<|> getRoutes
    :<|> snapToRoad

autoComplete :: Id Person.Person -> DMaps.AutoCompleteReq -> FlowHandler DMaps.AutoCompleteResp
autoComplete personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.autoComplete personId

getPlaceDetails :: Id Person.Person -> DMaps.GetPlaceDetailsReq -> FlowHandler DMaps.GetPlaceDetailsResp
getPlaceDetails personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.getPlaceDetails personId

getPlaceName :: Id Person.Person -> DMaps.GetPlaceNameReq -> FlowHandler DMaps.GetPlaceNameResp
getPlaceName personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.getPlaceName personId

getDistance :: Id Person.Person -> DMaps.GetDistanceReq -> FlowHandler DMaps.GetDistanceResp
getDistance personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.getDistance personId

getRoutes :: Id Person.Person -> DMaps.GetRoutesReq -> FlowHandler DMaps.GetRoutesResp
getRoutes personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.getRoutes personId

snapToRoad :: Id Person.Person -> DMaps.SnapToRoadReq -> FlowHandler DMaps.SnapToRoadResp
snapToRoad personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.snapToRoad personId
