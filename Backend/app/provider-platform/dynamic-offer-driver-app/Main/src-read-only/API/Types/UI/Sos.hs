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

data MarkAsSafeReq = MarkAsSafeReq {contacts :: Kernel.Prelude.Maybe [Data.Text.Text], isEndLiveTracking :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, isRideEnded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosDetailsRes = SosDetailsRes {sos :: Kernel.Prelude.Maybe Safety.Domain.Types.Sos.Sos}
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

data SosRes = SosRes {sosId :: Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
