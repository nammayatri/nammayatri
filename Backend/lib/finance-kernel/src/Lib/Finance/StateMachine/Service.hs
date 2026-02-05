{-
  Finance.StateMachine.Service

  Concrete state machine operations for domain use.
  Implements LAW 3: Deterministic State Transitions.
  Uses generated Beam queries internally.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.StateMachine.Service
  ( -- * State machine operations
    transition,
    getCurrentState,
    getStateHistory,
    initializeState,
    isValidTransition,

    -- * Input types (re-export from Interface)
    module Lib.Finance.StateMachine.Interface,
  )
where

import qualified Data.Map.Strict as M
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.CurrentState (CurrentState (..))
import Lib.Finance.Domain.Types.StateTransition
import Lib.Finance.Error.Types
import Lib.Finance.StateMachine.Interface
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.CurrentState as QCurrentState
import qualified Lib.Finance.Storage.Queries.StateTransition as QTransition

-- | Perform a state transition
-- Validates the transition is allowed, creates transition record, updates current state
transition ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Entity type
  Text -> -- Entity ID
  PaymentEvent -> -- Trigger event
  ActorInfo -> -- Who triggered it
  Maybe Value -> -- Metadata
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError StateTransition)
transition entityType entityId event actor metadata merchantId merchantOpCityId = do
  -- Get current state
  mbCurrentState <- QCurrentState.findByEntity entityType entityId

  case mbCurrentState of
    Nothing -> pure $ Left $ StateError EntityNotFound "Entity not initialized"
    Just currentStateRecord -> do
      let fromState = currentStateRecord.currentState

      -- Check if transition is valid
      case getNextState fromState event of
        Nothing ->
          pure $
            Left $
              StateError InvalidTransition $
                "Cannot transition from "
                  <> show fromState
                  <> " with event "
                  <> show event
        Just toState -> do
          now <- getCurrentTime
          transitionId <- generateGUID

          -- Create transition record (matching generated field names)
          let stateTransition =
                StateTransition
                  { id = Id transitionId,
                    entityType = entityType,
                    entityId = entityId,
                    fromState = fromState,
                    toState = toState,
                    event = event,
                    actorType = actor.actorType,
                    actorId = actor.actorId,
                    eventData = metadata,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOpCityId,
                    createdAt = now,
                    updatedAt = now
                  }

          QTransition.create stateTransition

          -- Update current state
          QCurrentState.updateState toState entityType entityId

          pure $ Right stateTransition

-- | Get current state for an entity
getCurrentState ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Entity type
  Text -> -- Entity ID
  m (Maybe PaymentState)
getCurrentState entityType entityId = do
  mbState <- QCurrentState.findByEntity entityType entityId
  pure $ mbState <&> (.currentState)

-- | Get full state history for an entity
getStateHistory ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Entity type
  Text -> -- Entity ID
  m [StateTransition]
getStateHistory = QTransition.findByEntity

-- | Initialize entity with a starting state
initializeState ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Entity type
  Text -> -- Entity ID
  PaymentState -> -- Initial state
  ActorInfo -> -- Actor who initialized
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError CurrentState)
initializeState entityType entityId initialState _actor merchantId merchantOpCityId = do
  -- Check if already initialized
  mbExisting <- QCurrentState.findByEntity entityType entityId

  case mbExisting of
    Just existing -> pure $ Right existing -- Already initialized, return existing
    Nothing -> do
      now <- getCurrentTime

      let currentState =
            CurrentState
              { entityType = entityType,
                entityId = entityId,
                currentState = initialState,
                merchantId = merchantId,
                merchantOperatingCityId = merchantOpCityId,
                createdAt = now,
                updatedAt = now
              }

      QCurrentState.create currentState
      pure $ Right currentState

-- | Check if a transition is valid
isValidTransition ::
  PaymentState ->
  PaymentEvent ->
  Bool
isValidTransition fromState event =
  isJust $ getNextState fromState event

-- | Get next state for a given (state, event) pair
-- Uses the default valid transitions from Interface
getNextState :: PaymentState -> PaymentEvent -> Maybe PaymentState
getNextState fromState event =
  M.lookup fromState defaultValidTransitions >>= M.lookup event
