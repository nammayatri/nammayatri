{-# OPTIONS_GHC -Wno-unused-imports #-}

module Safety.API.Types.UI.Sos where

import Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Safety.Domain.Types.Common
import qualified Safety.Domain.Types.Sos
import Servant

data CallPoliceAPI = CallPoliceAPI {rideId :: Kernel.Types.Id.Id Safety.Domain.Types.Common.Ride}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ErssStatusUpdateReq = ErssStatusUpdateReq
  { idSource :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idErss :: Kernel.Prelude.Text,
    currentStatus :: Kernel.Prelude.Text,
    statusDesc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    comments :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastUpdatedTime :: Kernel.Prelude.Maybe EulerHS.Prelude.Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ErssStatusUpdateRes = ErssStatusUpdateRes
  { resultCode :: Kernel.Prelude.Text,
    resultString :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorMsg :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    message :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payLoad :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MarkAsSafeReq = MarkAsSafeReq
  { contacts :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    isEndLiveTracking :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isMock :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isRideEnded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MockSosReq = MockSosReq {onRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, startDrill :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideDetailsForDriverRes = RideDetailsForDriverRes
  { driverName :: Kernel.Prelude.Text,
    trackingUrl :: Kernel.Prelude.Text,
    tripEndPos :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    tripStartPos :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosDetailsRes = SosDetailsRes {externalSOSConfig :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.ExternalSOSConfig, sos :: Kernel.Prelude.Maybe Safety.Domain.Types.Sos.Sos}
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
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Safety.Domain.Types.Common.Ride),
    sendPNOnPostRideSOS :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    triggerApiList :: Kernel.Prelude.Maybe [TriggerApi]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosRes = SosRes {externalSOSSuccess :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, kaptureTicketId :: Kernel.Prelude.Maybe Kernel.Prelude.Text, sosId :: Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosTrackingDetailsRes = SosTrackingDetailsRes {mobileNumber :: Kernel.Prelude.Text, personName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosTrackingRes = SosTrackingRes {currentLocation :: Kernel.Prelude.Maybe SosLocationRes, sosState :: Kernel.Prelude.Maybe Safety.Domain.Types.Sos.SosState, status :: Safety.Domain.Types.Sos.SosStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosUpdateReq = SosUpdateReq {comment :: Kernel.Prelude.Maybe Kernel.Prelude.Text, status :: Safety.Domain.Types.Sos.SosStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StartTrackingReq = StartTrackingReq
  { contacts :: [Kernel.Prelude.Text],
    customerLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    durationInMinutes :: Kernel.Prelude.Int,
    externalReferenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sosId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StartTrackingRes = StartTrackingRes {sosId :: Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos, trackingUrl :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TriggerApi
  = POLICE
  | KAPTURE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data UpdateStateReq = UpdateStateReq {sosState :: Safety.Domain.Types.Sos.SosState}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateToRideReq = UpdateToRideReq {rideId :: Kernel.Types.Id.Id Safety.Domain.Types.Common.Ride}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum (''TriggerApi))
