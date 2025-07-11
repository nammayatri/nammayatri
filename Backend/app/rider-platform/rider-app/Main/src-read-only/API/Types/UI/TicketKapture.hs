{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.TicketKapture where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data ActiveTicketsRes = ActiveTicketsRes {rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride), ticketId :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetAllActiveTicketsRes = GetAllActiveTicketsRes {activeTickets :: [ActiveTicketsRes]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TicketKaptureResp = TicketKaptureResp {encryptedCc :: Data.Text.Text, encryptedIv :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
