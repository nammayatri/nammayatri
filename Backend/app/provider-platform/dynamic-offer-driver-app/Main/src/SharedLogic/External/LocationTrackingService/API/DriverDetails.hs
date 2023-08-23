{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.API.DriverDetails where

import qualified EulerHS.Types as ET
import Kernel.Types.APISuccess (APISuccess)
import Servant
import SharedLogic.External.LocationTrackingService.Types

type LocationTrackingServiceAPI =
  "internal"
    :> "driver"
    :> "driverDetails"
    :> ReqBody '[JSON] DriverDetailsReq
    :> Post '[JSON] APISuccess

locationTrackingServiceAPI :: Proxy LocationTrackingServiceAPI
locationTrackingServiceAPI = Proxy

driverDetails :: DriverDetailsReq -> ET.EulerClient APISuccess
driverDetails = ET.client locationTrackingServiceAPI
