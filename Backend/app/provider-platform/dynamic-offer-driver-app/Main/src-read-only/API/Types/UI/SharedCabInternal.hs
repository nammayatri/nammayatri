{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCabInternal where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.DriverRouteTrip
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data SharedCabCheckInReq = SharedCabCheckInReq {driverId :: Kernel.Prelude.Text, routeCode :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabDemandNudgeReq = SharedCabDemandNudgeReq {onRoutePlates :: [Kernel.Prelude.Text], routes :: [SharedCabNudgeRoute], tunables :: SharedCabNudgeTunables}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabDriverCallNumberResp = SharedCabDriverCallNumberResp {driverNumber :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabDriverRouteTripCount = SharedCabDriverRouteTripCount {routeCode :: Kernel.Prelude.Text, tripCount :: Kernel.Prelude.Int}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabDriverRouteTripReq = SharedCabDriverRouteTripReq
  { driverId :: Kernel.Prelude.Text,
    idempotencyKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    source :: Kernel.Prelude.Maybe Domain.Types.DriverRouteTrip.DriverRouteTripSource,
    vehicleNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabNudgeRoute = SharedCabNudgeRoute {ahead :: [Kernel.Prelude.Int], onRoute :: [SharedCabOnRouteCab], routeCode :: Kernel.Prelude.Text, routeName :: Kernel.Prelude.Text, stops :: [SharedCabNudgeStop]}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabNudgeStop = SharedCabNudgeStop {code :: Kernel.Prelude.Text, name :: Kernel.Prelude.Text, point :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabNudgeTunables = SharedCabNudgeTunables
  { expiresAt :: Kernel.Prelude.UTCTime,
    idleRadiusM :: Kernel.Prelude.Int,
    maxIdlePerRoute :: Kernel.Prelude.Int,
    maxLookups :: Kernel.Prelude.Int,
    maxOptions :: Kernel.Prelude.Int,
    mergeRadiusM :: Kernel.Prelude.Int,
    minRiders :: Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabOnRouteCab = SharedCabOnRouteCab {nextIdx :: Kernel.Prelude.Int, vehicleNumber :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabTrip = SharedCabTrip
  { agencyId :: Kernel.Prelude.Text,
    capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    driverId :: Kernel.Prelude.Text,
    id :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    startedAt :: Kernel.Prelude.UTCTime,
    vehicleNumber :: Kernel.Prelude.Text,
    walkupCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
