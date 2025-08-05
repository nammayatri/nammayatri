{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.Types.FRFSTicketStatus where

import Data.Aeson
import Kernel.Prelude

data FRFSTicketStatus
  = ACTIVE
  | EXPIRED
  | USED
  | CANCELLED
  | COUNTER_CANCELLED
  | CANCEL_INITIATED
  | TECHNICAL_CANCEL_REJECTED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)
