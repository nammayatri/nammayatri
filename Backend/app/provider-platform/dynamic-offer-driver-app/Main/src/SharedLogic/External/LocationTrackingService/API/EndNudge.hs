module SharedLogic.External.LocationTrackingService.API.EndNudge where

import qualified Domain.Types.DriverNudge as DN
import qualified EulerHS.Types as ET
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Servant
import SharedLogic.External.LocationTrackingService.Types

type LocationTrackingServiceAPI =
  "nudge"
    :> Capture "nudgeId" (Id DN.DriverNudge)
    :> "end"
    :> ReqBody '[JSON] EndNudgeReq
    :> Post '[JSON] APISuccess

locationTrackingServiceAPI :: Proxy LocationTrackingServiceAPI
locationTrackingServiceAPI = Proxy

endNudge :: Id DN.DriverNudge -> EndNudgeReq -> ET.EulerClient APISuccess
endNudge = ET.client locationTrackingServiceAPI
