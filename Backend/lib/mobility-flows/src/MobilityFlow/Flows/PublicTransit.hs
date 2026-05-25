{-
  Public transit (FRFS) flow state machine definition.
-}
module MobilityFlow.Flows.PublicTransit
  ( TicketFlowState (..),
  )
where

import Kernel.Prelude

-- | All possible states of a FRFS ticket booking flow
data TicketFlowState
  = TFIdle
  | TFSearching
  | TFCatalogReceived
  | TFSelected
  | TFPaymentPending
  | TFConfirming
  | TFConfirmed
  | TFCancelled
  | TFExpired
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)
