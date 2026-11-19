{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.NavigationRoute
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.NavigationRoute as DNavigationRoute
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

-- | @POST /ui/navigation/route@: OSRM's full route (every turn step) for the driver app's in-app navigation.
type API =
  "navigation"
    :> "route"
    :> TokenAuth
    :> ReqBody '[JSON] DNavigationRoute.NavigationRouteReq
    :> Post '[JSON] Value

handler :: FlowServer API
handler = getNavigationRoute

getNavigationRoute :: (Id Person.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DNavigationRoute.NavigationRouteReq -> FlowHandler Value
getNavigationRoute auth@(personId, _, _) = withFlowHandlerAPI . withPersonIdLogTag personId . DNavigationRoute.getNavigationRoute auth
