{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCabInternal where

import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
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

data RiderCallNumberResp = RiderCallNumberResp {riderNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelectRouteResp = SelectRouteResp {affectedRiders :: Kernel.Prelude.Maybe [AffectedRider], session :: Kernel.Prelude.Maybe SharedLogic.SharedCab.SessionView.SharedCabSession}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabBookingOperation
  = ACCEPT
  | DECLINE
  | CANCEL
  | BOARDED_WITHOUT_CODE
  | DROPPED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabBookingOperationReq = SharedCabBookingOperationReq {driverId :: Kernel.Prelude.Text, operation :: SharedCabBookingOperation, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text, vehicleNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabDemandReq = SharedCabDemandReq {agencyId :: Kernel.Prelude.Text, routeCodes :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabMessageReq = SharedCabMessageReq {driverId :: Kernel.Prelude.Text, message :: Kernel.Prelude.Text, vehicleNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabRouteOperation
  = SELECT_ROUTE
  | RETURN_TRIP
  | CHANGE_ROUTE
  | END_ROUTE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabRouteOperationReq = SharedCabRouteOperationReq
  { agencyId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    atLastStop :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    driverId :: Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    forceEnd :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    mode :: Kernel.Prelude.Maybe SharedLogic.SharedCab.SessionState.SelectRouteMode,
    operation :: SharedCabRouteOperation,
    routeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    walkupCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabSessionOperation
  = SET_SEATS
  | RESUME
  | CAB_FULL
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabSessionOperationReq = SharedCabSessionOperationReq
  { driverId :: Kernel.Prelude.Text,
    operation :: SharedCabSessionOperation,
    vehicleNumber :: Kernel.Prelude.Text,
    version :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    walkupCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
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
