{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.SavedReqLocation
  ( API,
    handler,
    DSavedReqLocation.CreateSavedReqLocationReq (..),
    DSavedReqLocation.SavedReqLocationsListRes (..),
  )
where

import qualified Domain.Action.UI.SavedReqLocation as DSavedReqLocation
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (state)
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "savedLocation"
    :> ( TokenAuth
           :> ReqBody '[JSON] DSavedReqLocation.CreateSavedReqLocationReq
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> "list"
             :> Get '[JSON] DSavedReqLocation.SavedReqLocationsListRes
           :<|> TokenAuth
             :> Capture "tag" Text
             :> Delete '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  createSavedReqLocation
    :<|> getSavedReqLocations
    :<|> deleteSavedReqLocation

createSavedReqLocation :: (Id Person.Person, Id Merchant.Merchant) -> DSavedReqLocation.CreateSavedReqLocationReq -> FlowHandler APISuccess.APISuccess
createSavedReqLocation (riderId, _) = withFlowHandlerAPI . withPersonIdLogTag riderId . DSavedReqLocation.createSavedReqLocation riderId

getSavedReqLocations :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler DSavedReqLocation.SavedReqLocationsListRes
getSavedReqLocations (riderId, _) = withFlowHandlerAPI . withPersonIdLogTag riderId $ DSavedReqLocation.getSavedReqLocations riderId

deleteSavedReqLocation :: (Id Person.Person, Id Merchant.Merchant) -> Text -> FlowHandler APISuccess.APISuccess
deleteSavedReqLocation (riderId, _) = withFlowHandlerAPI . withPersonIdLogTag riderId . DSavedReqLocation.deleteSavedReqLocation riderId
