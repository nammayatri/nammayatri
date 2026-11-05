{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCabInternal where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data RiderCallNumberResp = RiderCallNumberResp {riderNumber :: Kernel.Prelude.Text}
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

data SharedCabSessionOperation
  = SET_SEATS
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
