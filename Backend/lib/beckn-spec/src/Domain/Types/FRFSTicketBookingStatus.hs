{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.Types.FRFSTicketBookingStatus where

import Data.Aeson
import Kernel.Prelude

data FRFSTicketBookingStatus
  = NEW
  | APPROVED
  | PAYMENT_PENDING
  | CONFIRMING
  | FAILED
  | CONFIRMED
  | CANCELLED
  | COUNTER_CANCELLED
  | CANCEL_INITIATED
  | TECHNICAL_CANCEL_REJECTED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)
