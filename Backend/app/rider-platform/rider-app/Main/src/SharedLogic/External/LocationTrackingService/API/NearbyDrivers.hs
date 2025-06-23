module SharedLogic.External.LocationTrackingService.API.NearbyDrivers where

import qualified EulerHS.Types as ET
import Servant
import SharedLogic.External.LocationTrackingService.Types

type LocationTrackingServiceAPI =
  "internal"
    :> "drivers"
    :> "nearby"
    :> ReqBody '[JSON] NearByDriverReq
    :> Get '[JSON] [NearByDriverRes]

locationTrackingServiceAPI :: Proxy LocationTrackingServiceAPI
locationTrackingServiceAPI = Proxy

nearBy :: NearByDriverReq -> ET.EulerClient [NearByDriverRes]
nearBy = ET.client locationTrackingServiceAPI
