{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Shared so both rider-app and driver-app use one definition. Only the FRFS arms: these are
-- the ones getFRFSAllStatuses can produce for a ticket booking.
module Domain.Types.JourneyBookingStatus where

import Data.Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import EulerHS.Prelude

data FeedbackStatus = FEEDBACK_PENDING
  deriving (Generic, ToSchema, Show, Eq, Ord)

data JourneyBookingStatus
  = FRFSBooking DFRFSBooking.FRFSTicketBookingStatus
  | FRFSTicket DFRFSTicket.FRFSTicketStatus
  | Feedback FeedbackStatus
  deriving (Generic, ToSchema, ToJSON, FromJSON, Show, Eq, Ord)

instance FromJSON FeedbackStatus where
  parseJSON _ = return FEEDBACK_PENDING

instance ToJSON FeedbackStatus where
  toJSON FEEDBACK_PENDING = "FEEDBACK_PENDING"
