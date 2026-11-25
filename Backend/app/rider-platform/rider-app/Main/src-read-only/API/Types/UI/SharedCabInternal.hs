{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCabInternal where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data NearbyStandDemand = NearbyStandDemand {lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double, searching :: Kernel.Prelude.Int, stopCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderCallNumberResp = RiderCallNumberResp {riderNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabBookingOperation
  = ACCEPT
  | DECLINE
  | CANCEL
  | BOARDED_WITHOUT_CODE
  | BOARDED_WITH_OTP
  | NO_SHOW
  | DROPPED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabBookingOperationReq = SharedCabBookingOperationReq
  { driverId :: Kernel.Prelude.Text,
    operation :: SharedCabBookingOperation,
    otp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabDemandReq = SharedCabDemandReq {agencyId :: Kernel.Prelude.Text, routeCodes :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabMessageReq = SharedCabMessageReq {driverId :: Kernel.Prelude.Text, message :: Kernel.Prelude.Text, vehicleNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedRunPassenger = SharedRunPassenger
  { boardStopCode :: Kernel.Prelude.Text,
    boarded :: Kernel.Prelude.Bool,
    dropStopCode :: Kernel.Prelude.Text,
    fare :: Kernel.Types.Common.HighPrecMoney,
    firstName :: Kernel.Prelude.Text,
    seats :: Kernel.Prelude.Int,
    ticketBookingId :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
