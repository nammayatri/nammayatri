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
    autoComplete,
    getPlaceDetails,
    getPlaceName,
  )
where

import qualified Domain.Action.UI.Maps as DMaps
import qualified Domain.Types.Merchant as Merchant
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
       )

handler :: FlowServer API
handler =
  autoComplete
    :<|> getPlaceDetails
    :<|> getPlaceName

autoComplete :: (Id Person.Person, Id Merchant.Merchant) -> DMaps.AutoCompleteReq -> FlowHandler DMaps.AutoCompleteResp
autoComplete (personId, merchantId) = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.autoComplete (personId, merchantId)

getPlaceDetails :: (Id Person.Person, Id Merchant.Merchant) -> DMaps.GetPlaceDetailsReq -> FlowHandler DMaps.GetPlaceDetailsResp
getPlaceDetails (personId, merchantId) = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.getPlaceDetails (personId, merchantId)

getPlaceName :: (Id Person.Person, Id Merchant.Merchant) -> DMaps.GetPlaceNameReq -> FlowHandler DMaps.GetPlaceNameResp
getPlaceName (personId, merchantId) = withFlowHandlerAPI . withPersonIdLogTag personId . DMaps.getPlaceName (personId, merchantId)
