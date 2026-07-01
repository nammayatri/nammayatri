{-
  Finance.StateMachine.Interface

  Input types and helpers for state machine operations.
  The actual operations are in Lib.Finance.StateMachine.Service
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.StateMachine.Interface
  ( defaultValidTransitions,
  )
where

import qualified Data.Map.Strict as M
import Lib.Finance.Domain.Types.StateTransition (PaymentEvent (..), PaymentState (..))

-- | Default valid transitions (Map from state -> event -> next state)
defaultValidTransitions :: M.Map PaymentState (M.Map PaymentEvent PaymentState)
defaultValidTransitions =
  M.fromList
    [ (PENDING, M.fromList [(AUTHORIZE, AUTHORIZED), (FAIL, FAILED), (CANCEL, CANCELLED)]),
      (AUTHORIZED, M.fromList [(CAPTURE, CAPTURED), (FAIL, FAILED), (CANCEL, CANCELLED)]),
      (CAPTURED, M.fromList [(SETTLE, SETTLED), (REFUND, REFUNDED), (FAIL, FAILED)]),
      (SETTLED, M.fromList [(REFUND, REFUNDED)])
      -- FAILED, REFUNDED, CANCELLED are terminal states (no outgoing transitions)
    ]
