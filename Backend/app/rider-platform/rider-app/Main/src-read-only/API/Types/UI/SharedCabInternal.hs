{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCabInternal where

import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.VehicleTrip
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import qualified SharedLogic.SharedCab.SessionState
import qualified SharedLogic.SharedCab.SessionView
import Tools.Auth

data AffectedRider = AffectedRider {bookingId :: Kernel.Prelude.Text, dropStop :: Kernel.Prelude.Text, firstName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EndRouteNext
  = RETURN
  | CHANGE
  | END
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EndRouteReq = EndRouteReq
  { atLastStop :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    driverId :: Kernel.Prelude.Text,
    force :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    next :: EndRouteNext,
    vehicleNumber :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SeatsReq = SeatsReq {driverId :: Kernel.Prelude.Text, vehicleNumber :: Kernel.Prelude.Text, version :: Kernel.Prelude.Int, walkupCount :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelectRouteReq = SelectRouteReq
  { capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    driverId :: Kernel.Prelude.Text,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    mode :: Kernel.Prelude.Maybe SharedLogic.SharedCab.SessionState.SelectRouteMode,
    routeCode :: Kernel.Prelude.Text,
    serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType,
    vehicleNumber :: Kernel.Prelude.Text,
    walkupCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelectRouteResp = SelectRouteResp {affectedRiders :: Kernel.Prelude.Maybe [AffectedRider], session :: Kernel.Prelude.Maybe SharedLogic.SharedCab.SessionView.SharedCabSession}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabDriverReq = SharedCabDriverReq {driverId :: Kernel.Prelude.Text, vehicleNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabRoutesResp = SharedCabRoutesResp {routes :: [SharedLogic.SharedCab.SessionView.SharedCabRoute]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabTrip = SharedCabTrip
  { endReason :: Kernel.Prelude.Maybe Domain.Types.VehicleTrip.VehicleTripEndReason,
    endedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip,
    offlineBoardings :: Kernel.Prelude.Int,
    routeCode :: Kernel.Prelude.Text,
    startedAt :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.VehicleTrip.VehicleTripStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabTripsResp = SharedCabTripsResp {trips :: [SharedCabTrip]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
