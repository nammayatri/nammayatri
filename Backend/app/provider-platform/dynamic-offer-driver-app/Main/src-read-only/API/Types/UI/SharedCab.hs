{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCab where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

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

data SharedCabBookingOperationReq = SharedCabBookingOperationReq {operation :: SharedCabBookingOperation, otp :: Kernel.Prelude.Maybe Kernel.Prelude.Text, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabMessageReq = SharedCabMessageReq {message :: Kernel.Prelude.Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
