{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Handler where

import qualified API.Types as API
import qualified Domain.Action.SpecialZone as DSpecialZone
import qualified Domain.Types.SpecialZone as Domain
import Environment
import EulerHS.Prelude hiding (id, state)
import Kernel.External.Maps (LatLong)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant hiding (throwError)

dashboardHandler :: FlowServer API.SpecialZoneDashboardAPIs
dashboardHandler _dashboard =
  lookupSpecialZonesByRegion
    :<|> createSpecialZone
    :<|> updateSpecialZone
    :<|> deleteSpecialZone

specialZoneHandler :: FlowServer API.SpecialZoneAPIs
specialZoneHandler = lookupSpecialZone

handler :: FlowServer API.API
handler = specialZoneHandler :<|> dashboardHandler

lookupSpecialZone :: LatLong -> FlowHandler Domain.SpecialZone
lookupSpecialZone latLng = withFlowHandlerAPI' $ DSpecialZone.lookupSpecialZone latLng

lookupSpecialZonesByRegion :: LatLong -> LatLong -> FlowHandler [Domain.SpecialZone]
lookupSpecialZonesByRegion minLatLng maxLatLng = withFlowHandlerAPI' $ DSpecialZone.lookupSpecialZonesByRegion minLatLng maxLatLng

createSpecialZone :: Domain.SpecialZoneAPIEntity -> FlowHandler APISuccess
createSpecialZone specialZone = withFlowHandlerAPI' $ DSpecialZone.createSpecialZone specialZone

updateSpecialZone :: Domain.SpecialZone -> FlowHandler APISuccess
updateSpecialZone specialZone = withFlowHandlerAPI' $ DSpecialZone.updateSpecialZone specialZone

deleteSpecialZone :: Id Domain.SpecialZone -> FlowHandler APISuccess
deleteSpecialZone specialZoneId = withFlowHandlerAPI' $ DSpecialZone.deleteSpecialZone specialZoneId
