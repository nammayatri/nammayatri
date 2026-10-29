module SharedLogic.External.LocationTrackingService.API.Ride where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Servant
import SharedLogic.External.LocationTrackingService.Types

type RideStartAPI =
  "internal"
    :> "ride"
    :> Capture "rideId" Text
    :> "start"
    :> ReqBody '[JSON] RideStartReq
    :> Post '[JSON] APISuccess

type RideEndAPI =
  "internal"
    :> "ride"
    :> Capture "rideId" Text
    :> "end"
    :> ReqBody '[JSON] RideEndReq
    :> Post '[JSON] RideEndRes

rideStartAPI :: Proxy RideStartAPI
rideStartAPI = Proxy

rideEndAPI :: Proxy RideEndAPI
rideEndAPI = Proxy

rideStart :: Text -> RideStartReq -> ET.EulerClient APISuccess
rideStart = ET.client rideStartAPI

rideEnd :: Text -> RideEndReq -> ET.EulerClient RideEndRes
rideEnd = ET.client rideEndAPI
