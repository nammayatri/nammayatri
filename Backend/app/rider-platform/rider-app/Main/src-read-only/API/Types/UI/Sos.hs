{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Sos where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Ride
import qualified Domain.Types.Sos
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data CallPoliceAPI = CallPoliceAPI {rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MarkAsSafeReq = MarkAsSafeReq {isMock :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, isRideEnded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MockSosReq = MockSosReq {onRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, startDrill :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosDetailsRes = SosDetailsRes {sos :: Kernel.Prelude.Maybe Domain.Types.Sos.Sos}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosReq = SosReq
  { customerLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    flow :: Domain.Types.Sos.SosType,
    isRideEnded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    notifyAllContacts :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    sendPNOnPostRideSOS :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosRes = SosRes {sosId :: Kernel.Types.Id.Id Domain.Types.Sos.Sos}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosUpdateReq = SosUpdateReq {comment :: Kernel.Prelude.Maybe Data.Text.Text, status :: Domain.Types.Sos.SosStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
