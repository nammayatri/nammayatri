module Domain.Action.Internal.LiveActivityUpdate where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import qualified SharedLogic.CallBAPInternal as CallBAPInternal

data LiveActivityUpdateReq = LiveActivityUpdateReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    driverLat :: Double,
    driverLon :: Double,
    rideNotificationStatus :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

liveActivityUpdate :: LiveActivityUpdateReq -> Flow APISuccess
liveActivityUpdate req = do
  appBackendBapInternal <- asks (.appBackendBapInternal)
  let phase = maybe "INPROGRESS" (const "NEW") req.rideNotificationStatus
      liveActivityReq =
        CallBAPInternal.DriverLiveActivityReq
          { bppRideId = req.rideId.getId,
            driverLat = req.driverLat,
            driverLon = req.driverLon,
            ridePhase = phase
          }
  void $ CallBAPInternal.driverLiveActivity appBackendBapInternal.apiKey appBackendBapInternal.url liveActivityReq
  pure Success
