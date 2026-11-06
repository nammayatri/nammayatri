{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.EditBooking where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data EditBookingRespondAPIReq = EditBookingRespondAPIReq {action :: EditBookingRespondAction, isTimeout :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EditBookingRespondAction
  = ACCEPT
  | REJECT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
