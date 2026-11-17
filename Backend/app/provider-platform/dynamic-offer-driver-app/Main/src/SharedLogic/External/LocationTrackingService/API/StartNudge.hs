module SharedLogic.External.LocationTrackingService.API.StartNudge where

import qualified Domain.Types.DriverNudge as DN
import qualified EulerHS.Types as ET
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Servant
import SharedLogic.External.LocationTrackingService.Types

type LocationTrackingServiceAPI =
  "nudge"
    :> Capture "nudgeId" (Id DN.DriverNudge)
    :> "start"
    :> ReqBody '[JSON] StartNudgeReq
    :> Post '[JSON] APISuccess

locationTrackingServiceAPI :: Proxy LocationTrackingServiceAPI
locationTrackingServiceAPI = Proxy

startNudge :: Id DN.DriverNudge -> StartNudgeReq -> ET.EulerClient APISuccess
startNudge = ET.client locationTrackingServiceAPI
