{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.RegistryMap where

import qualified Domain.Action.Dashboard.RegistryMap as DRM
import qualified Domain.Types.Merchant as DM
import Domain.Types.RegistryMapFallback
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant

data UpdateRegistryMapEndpoint = RegistryMapEndpoint
  deriving (Show, Read)

derivePersistField "UpdateRegistryMapEndpoint"

type API =
  "registry"
    :> ReqBody '[JSON] RegistryMapReq
    :> Post '[JSON] APISuccess

handler :: ShortId DM.Merchant -> FlowServer API
handler = updateRegistry

updateRegistry :: ShortId DM.Merchant -> RegistryMapReq -> FlowHandler APISuccess
updateRegistry _ req = withFlowHandlerAPI $ DRM.updateRegistry req
