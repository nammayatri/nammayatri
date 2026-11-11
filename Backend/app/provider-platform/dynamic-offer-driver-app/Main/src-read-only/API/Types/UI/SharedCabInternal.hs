{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCabInternal where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data SharedCabCheckInReq = SharedCabCheckInReq {driverId :: Kernel.Prelude.Text, routeCode :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabDriverCallNumberResp = SharedCabDriverCallNumberResp {driverNumber :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabMoveWarningReq = SharedCabMoveWarningReq {boardStop :: Kernel.Prelude.Maybe Kernel.Prelude.Text, bookingId :: Kernel.Prelude.Text, driverId :: Kernel.Prelude.Text, releaseInSec :: Kernel.Prelude.Int}
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
