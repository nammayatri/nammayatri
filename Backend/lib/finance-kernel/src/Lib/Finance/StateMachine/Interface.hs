{-
  Finance.StateMachine.Interface

  Input types and helpers for state machine operations.
  The actual operations are in Lib.Finance.StateMachine.Service
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.StateMachine.Interface
  ( ActorInfo (..),
    defaultValidTransitions,
  )
where

import qualified Data.Map.Strict as M
import Kernel.Prelude
import Lib.Finance.Domain.Types.StateTransition (PaymentEvent (..), PaymentState (..))

-- | Actor information for state transitions
data ActorInfo = ActorInfo
  { actorType :: Text, -- "SYSTEM", "USER", "ADMIN", "JOB"
    actorId :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default valid transitions (Map from state -> event -> next state)
defaultValidTransitions :: M.Map PaymentState (M.Map PaymentEvent PaymentState)
defaultValidTransitions =
  M.fromList
    [ (Pending, M.fromList [(Authorize, Authorized), (Fail, Failed), (Cancel, Cancelled)]),
      (Authorized, M.fromList [(Capture, Captured), (Fail, Failed), (Cancel, Cancelled)]),
      (Captured, M.fromList [(Settle, Settled), (Refund, Refunded), (Fail, Failed)]),
      (Settled, M.fromList [(Refund, Refunded)])
      -- Failed, Refunded, Cancelled are terminal states (no outgoing transitions)
    ]
