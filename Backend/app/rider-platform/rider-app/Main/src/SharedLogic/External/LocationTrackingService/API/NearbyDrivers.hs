module SharedLogic.External.LocationTrackingService.API.NearbyDrivers where

import Domain.Types.DriverLocation
import qualified EulerHS.Types as ET
import Servant
import SharedLogic.External.LocationTrackingService.Types

type LocationTrackingServiceAPI =
  "internal"
    :> "drivers"
    :> "nearby"
    :> ReqBody '[JSON] NearByReq
    :> Get '[JSON] [DriverLocation]

locationTrackingServiceAPI :: Proxy LocationTrackingServiceAPI
locationTrackingServiceAPI = Proxy

nearBy :: NearByReq -> ET.EulerClient [DriverLocation]
nearBy = ET.client locationTrackingServiceAPI
