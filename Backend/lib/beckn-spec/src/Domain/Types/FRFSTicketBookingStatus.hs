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

-- | Validates whether a state transition is legal in the FRFS booking state machine.
-- Returns True if the transition from the first status to the second is allowed.
isValidTransition :: FRFSTicketBookingStatus -> FRFSTicketBookingStatus -> Bool
isValidTransition from to
  | from == to = True -- Identity transitions are always valid (idempotent)
  | otherwise = case (from, to) of
      -- Forward flow
      (NEW, APPROVED) -> True
      (NEW, PAYMENT_PENDING) -> True
      (APPROVED, PAYMENT_PENDING) -> True
      (PAYMENT_PENDING, CONFIRMING) -> True
      (CONFIRMING, CONFIRMED) -> True
      -- Failure transitions (any non-terminal state can fail)
      (NEW, FAILED) -> True
      (APPROVED, FAILED) -> True
      (PAYMENT_PENDING, FAILED) -> True
      (CONFIRMING, FAILED) -> True
      -- Cancellation transitions
      (NEW, CANCEL_INITIATED) -> True
      (APPROVED, CANCEL_INITIATED) -> True
      (PAYMENT_PENDING, CANCEL_INITIATED) -> True
      (CONFIRMING, CANCEL_INITIATED) -> True
      (CONFIRMED, CANCEL_INITIATED) -> True
      (CANCEL_INITIATED, CANCELLED) -> True
      (CANCEL_INITIATED, COUNTER_CANCELLED) -> True
      (CANCELLED, COUNTER_CANCELLED) -> True
      (CANCEL_INITIATED, TECHNICAL_CANCEL_REJECTED) -> True
      -- Recovery transitions
      (FAILED, PAYMENT_PENDING) -> True -- Retry after failure
      -- All other transitions are invalid
      _ -> False

-- | Checks if a status is terminal (no further transitions expected)
isTerminalStatus :: FRFSTicketBookingStatus -> Bool
isTerminalStatus CONFIRMED = True
isTerminalStatus FAILED = True
isTerminalStatus CANCELLED = True
isTerminalStatus COUNTER_CANCELLED = True
isTerminalStatus TECHNICAL_CANCEL_REJECTED = True
isTerminalStatus _ = False
