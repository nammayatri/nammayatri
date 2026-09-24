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
import Tools.Auth

data SharedCabChangeRouteReq = SharedCabChangeRouteReq {driverId :: Kernel.Prelude.Text, routeCode :: Kernel.Prelude.Text, vehicleNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabDriverReq = SharedCabDriverReq {driverId :: Kernel.Prelude.Text, vehicleNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabEndNext
  = RETURN
  | CHANGE
  | END
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabEndReq = SharedCabEndReq {driverId :: Kernel.Prelude.Text, next :: SharedCabEndNext, routeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, vehicleNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabRouteResp = SharedCabRouteResp {code :: Kernel.Prelude.Text, distanceMeters :: Kernel.Prelude.Double, longName :: Kernel.Prelude.Text, shortName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabSeatsReq = SharedCabSeatsReq {driverId :: Kernel.Prelude.Text, vehicleNumber :: Kernel.Prelude.Text, version :: Kernel.Prelude.Int, walkupCount :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabSelectReq = SharedCabSelectReq
  { capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    driverId :: Kernel.Prelude.Text,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    routeCode :: Kernel.Prelude.Text,
    serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType,
    vehicleNumber :: Kernel.Prelude.Text,
    walkupCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabSessionResp = SharedCabSessionResp
  { available :: Kernel.Prelude.Int,
    capacity :: Kernel.Prelude.Int,
    pauseReason :: Kernel.Prelude.Maybe SharedLogic.SharedCab.SessionState.PauseReason,
    queuedRouteCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    startedAt :: Kernel.Prelude.UTCTime,
    status :: SharedLogic.SharedCab.SessionState.SessionStatus,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleTripId :: Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip,
    version :: Kernel.Prelude.Int,
    walkupCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
