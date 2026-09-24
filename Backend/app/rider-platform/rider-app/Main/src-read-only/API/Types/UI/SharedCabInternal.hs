{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCabInternal where

import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
import qualified Domain.Types.IntegratedBPPConfig
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import qualified SharedLogic.SharedCab.SessionState
import Tools.Auth

data AffectedRider = AffectedRider {bookingId :: Kernel.Prelude.Text, dropStop :: Kernel.Prelude.Text, firstName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AlightingRider = AlightingRider {bookingId :: Kernel.Prelude.Text, firstName :: Kernel.Prelude.Text, seats :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BoardingRider = BoardingRider
  { bookingId :: Kernel.Prelude.Text,
    dropStop :: Kernel.Prelude.Text,
    expiresAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fare :: Kernel.Types.Common.HighPrecMoney,
    firstName :: Kernel.Prelude.Text,
    minutesAway :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    riderStatus :: RiderStatus,
    seats :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DemandAtStop = DemandAtStop {searching :: Kernel.Prelude.Int, stopName :: Kernel.Prelude.Text, waiting :: Kernel.Prelude.Int, windowMin :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EndRouteNext
  = RETURN
  | CHANGE
  | END
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EndRouteReq = EndRouteReq {driverId :: Kernel.Prelude.Text, next :: EndRouteNext, vehicleNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LowDemandAlternative = LowDemandAlternative {distanceKm :: Kernel.Prelude.Double, name :: Kernel.Prelude.Text, riders :: Kernel.Prelude.Int, routeCode :: Kernel.Prelude.Text, windowMin :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LowDemandCard = LowDemandCard {alternatives :: [LowDemandAlternative]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OffRoute = OffRoute {nearestRoutes :: [SharedCabRoute]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderStatus
  = AT_STOP
  | MINUTES_AWAY
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RidersAtStop = RidersAtStop {alighting :: [AlightingRider], boarding :: [BoardingRider], stopName :: Kernel.Prelude.Text}
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

data SelectRouteResp = SelectRouteResp {affectedRiders :: Kernel.Prelude.Maybe [AffectedRider], session :: Kernel.Prelude.Maybe SharedCabSession}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SessionRoute = SessionRoute {code :: Kernel.Prelude.Text, direction :: Kernel.Prelude.Text, name :: Kernel.Prelude.Text, nextStops :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabDriverReq = SharedCabDriverReq {driverId :: Kernel.Prelude.Text, vehicleNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabRoute = SharedCabRoute
  { code :: Kernel.Prelude.Text,
    direction :: Kernel.Prelude.Text,
    distanceKm :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    fromStop :: Kernel.Prelude.Text,
    isStandRoute :: Kernel.Prelude.Bool,
    name :: Kernel.Prelude.Text,
    toStop :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabRoutesResp = SharedCabRoutesResp {routes :: [SharedCabRoute]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabSession = SharedCabSession
  { available :: Kernel.Prelude.Int,
    capacity :: Kernel.Prelude.Int,
    demandAhead :: [DemandAtStop],
    lowDemandCard :: Kernel.Prelude.Maybe LowDemandCard,
    movement :: SharedLogic.SharedCab.SessionState.SessionMovement,
    offRoute :: Kernel.Prelude.Maybe OffRoute,
    pauseReason :: Kernel.Prelude.Maybe SharedLogic.SharedCab.SessionState.PauseReason,
    queuedRoute :: Kernel.Prelude.Maybe SessionRoute,
    ridersByStop :: [RidersAtStop],
    route :: SessionRoute,
    status :: SharedLogic.SharedCab.SessionState.SessionStatus,
    version :: Kernel.Prelude.Int,
    walkupCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
