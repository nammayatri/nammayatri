{-
  Flow state machine definitions.

  A flow (ride-hailing, public transit) is an event-driven state machine.
  Each phase is triggered by a different event (rider API, Beckn callback, driver API).
  The state machine validates that transitions are legal before running handlers.
-}
module MobilityFlow.Core.StateMachine
  ( Trigger (..),
    FlowTransition (..),
    validateTransition,
  )
where

import Kernel.Prelude

-- | What can trigger a state transition
data Trigger
  = RiderAPI Text
  | DriverAPI Text
  | BecknCallback Text
  | SystemEvent Text
  deriving (Show, Eq, Ord, Generic)

-- | A single allowed transition in the state machine
data FlowTransition state = FlowTransition
  { fromStates :: [state],
    trigger :: Trigger,
    toState :: state,
    phaseName :: Text
  }
  deriving (Show, Generic)

-- | Validate whether a transition is allowed given current state and trigger
validateTransition ::
  (Eq state, Show state) =>
  [FlowTransition state] ->
  state ->
  Trigger ->
  Either Text (FlowTransition state)
validateTransition transitions currentState incomingTrigger =
  case filter matches transitions of
    [] ->
      Left $
        "No valid transition from state "
          <> show currentState
          <> " for trigger "
          <> show incomingTrigger
    (t : _) -> Right t
  where
    matches t = currentState `elem` t.fromStates && t.trigger == incomingTrigger
