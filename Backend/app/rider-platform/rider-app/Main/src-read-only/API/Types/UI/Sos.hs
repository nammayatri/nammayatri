{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Sos where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Safety.Domain.Types.Sos
import Servant
import Tools.Auth

data CallPoliceAPI = CallPoliceAPI {rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ErssStatusUpdateReq = ErssStatusUpdateReq
  { idSource :: Kernel.Prelude.Maybe Data.Text.Text,
    idErss :: Kernel.Prelude.Maybe Data.Text.Text,
    currentStatus :: Data.Text.Text,
    statusDesc :: Kernel.Prelude.Maybe Data.Text.Text,
    comments :: Kernel.Prelude.Maybe Data.Text.Text,
    lastUpdatedTime :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ErssStatusUpdateRes = ErssStatusUpdateRes
  { resultCode :: Data.Text.Text,
    resultString :: Kernel.Prelude.Maybe Data.Text.Text,
    errorMsg :: Kernel.Prelude.Maybe Data.Text.Text,
    message :: Kernel.Prelude.Maybe Data.Text.Text,
    payLoad :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MarkAsSafeReq = MarkAsSafeReq
  { contacts :: Kernel.Prelude.Maybe [Data.Text.Text],
    isEndLiveTracking :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isMock :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isRideEnded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MockSosReq = MockSosReq {onRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, startDrill :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosDetailsRes = SosDetailsRes {sos :: Kernel.Prelude.Maybe Safety.Domain.Types.Sos.Sos}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosLocationRes = SosLocationRes {accuracy :: Kernel.Prelude.Maybe Kernel.Prelude.Double, lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosLocationUpdateReq = SosLocationUpdateReq {accuracy :: Kernel.Prelude.Maybe Kernel.Prelude.Double, lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosReq = SosReq
  { customerLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    flow :: Safety.Domain.Types.Sos.SosType,
    isRideEnded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    notifyAllContacts :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride),
    sendPNOnPostRideSOS :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosRes = SosRes {externalSOSSuccess :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, sosId :: Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosTrackingDetailsRes = SosTrackingDetailsRes {mobileNumber :: Data.Text.Text, personName :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosTrackingRes = SosTrackingRes {currentLocation :: Kernel.Prelude.Maybe SosLocationRes, sosState :: Kernel.Prelude.Maybe Safety.Domain.Types.Sos.SosState, status :: Safety.Domain.Types.Sos.SosStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosUpdateReq = SosUpdateReq {comment :: Kernel.Prelude.Maybe Data.Text.Text, status :: Safety.Domain.Types.Sos.SosStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StartTrackingReq = StartTrackingReq
  { contacts :: [Data.Text.Text],
    customerLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    durationInMinutes :: Kernel.Prelude.Int,
    externalReferenceId :: Kernel.Prelude.Maybe Data.Text.Text,
    sosId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StartTrackingRes = StartTrackingRes {sosId :: Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos, trackingUrl :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateStateReq = UpdateStateReq {sosState :: Safety.Domain.Types.Sos.SosState}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateToRideReq = UpdateToRideReq {rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
