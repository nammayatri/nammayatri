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
           :<|> "getReverseGeocode"
             :> TokenAuth
             :> ReqBody '[JSON] DMaps.ReverseGeocodeReq
             :> Post '[JSON] DMaps.ReverseGeocodeResp
       )

handler :: FlowServer API
handler =
  autoComplete
    :<|> getPlaceDetails
    :<|> getPlaceName
    :<|> getReverseGeocode

autoComplete :: Id Person.Person -> DMaps.AutoCompleteReq -> FlowHandler DMaps.AutoCompleteResp
autoComplete personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.autoComplete personId

getPlaceDetails :: Id Person.Person -> DMaps.GetPlaceDetailsReq -> FlowHandler DMaps.GetPlaceDetailsResp
getPlaceDetails personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.getPlaceDetails personId

getPlaceName :: Id Person.Person -> DMaps.GetPlaceNameReq -> FlowHandler DMaps.GetPlaceNameResp
getPlaceName personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.getPlaceName personId

--This endpoint added for manual testing
getReverseGeocode :: Id Person.Person -> DMaps.ReverseGeocodeReq -> FlowHandler DMaps.ReverseGeocodeResp
getReverseGeocode personId = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.getReverseGeocode personId
