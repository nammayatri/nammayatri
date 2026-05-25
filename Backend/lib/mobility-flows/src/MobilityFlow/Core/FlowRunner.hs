{-
  FlowRunner: executes phases in both sync and async modes.

  Async mode (production):
  - runPhaseAsync: validates state → runs ONE handler → returns result
  - Used by the wiring layer (rider-app-v2, driver-app-v2) for each API endpoint.
  - Each API call runs one phase. The flow progresses via separate API calls.

  Sync mode (testing):
  - runSearchPhaseSync etc: chains BAP→ACL→BPP→ACL→BAP in one process
  - runFlowSync: chains ALL phases with FlowDecisions for rider/driver input
-}
module MobilityFlow.Core.FlowRunner
  ( -- * Async mode (production — one phase per API call)
    runPhaseAsync,

    -- * Single phase runners (sync mode)
    runSearchPhaseSync,
    runSelectPhaseSync,
    runConfirmPhaseSync,
    runStartRideSync,
    runEndRideSync,

    -- * Full flow runner (sync mode with decisions)
    runFlowSync,
    FlowResult (..),

    -- * State validation (used in both modes)
    validateAndTransition,
  )
where

import Kernel.Prelude
import MobilityFlow.Core.FlowDecisions (FlowDecisions (..))
import MobilityFlow.Core.PhaseLink (runLinkSync)
import MobilityFlow.Core.StateMachine
import MobilityFlow.Flows.RideHailing

-- ---------------------------------------------------------------------------
-- State validation
-- ---------------------------------------------------------------------------

validateAndTransition ::
  (Eq state, Show state) =>
  [FlowTransition state] ->
  state ->
  Trigger ->
  Either Text state
validateAndTransition transitions currentState trigger =
  case validateTransition transitions currentState trigger of
    Right t -> Right t.toState
    Left err -> Left err

-- ---------------------------------------------------------------------------
-- Async mode (production — one phase per API call)
-- ---------------------------------------------------------------------------

-- | Run a single phase in async mode.
--
-- This is the core building block for production wiring:
--   1. Resolves current flow state (from DB/Redis)
--   2. Validates the transition is legal
--   3. Runs the handler
--   4. Returns the result
--
-- Used by the wiring layer to turn a 3-step pattern into a one-liner:
--
-- @
-- -- Without runPhaseAsync (manual, repeated in every endpoint):
-- searchHandler ctx req = do
--   currentState <- resolveState personId
--   case validateAndTransition transitions currentState (RiderAPI "search") of
--     Left err -> throwError $ InvalidRequest err
--     Right _ -> pure ()
--   Search.search' ctx req
--
-- -- With runPhaseAsync (one-liner):
-- searchHandler ctx req =
--   runPhaseAsync transitions (resolveState personId) (RiderAPI "search") onError $
--     Search.search' ctx req
-- @
runPhaseAsync ::
  (Monad m, Eq state, Show state) =>
  -- | The state machine transitions
  [FlowTransition state] ->
  -- | How to resolve current state (e.g., DB lookup)
  m state ->
  -- | What triggered this phase (e.g., RiderAPI "search")
  Trigger ->
  -- | Error handler: called when transition is invalid
  (Text -> m a) ->
  -- | The actual handler to run if transition is valid
  m a ->
  m a
runPhaseAsync transitions resolveState trigger onError handler = do
  currentState <- resolveState
  case validateAndTransition transitions currentState trigger of
    Left err -> onError err
    Right _newState -> handler

-- ---------------------------------------------------------------------------
-- Single phase runners (sync mode — one Beckn round trip)
-- ---------------------------------------------------------------------------

-- | Run search phase: BAP search → ACL → BPP search → ACL → BAP onSearch
runSearchPhaseSync ::
  (Monad m) =>
  SearchPhase m bapCtx searchReq searchRes searchWire
    bppCtx bppSearchIn bppSearchRes
    onSearchWire onSearchIn estimate ->
  bapCtx ->
  bppCtx ->
  searchReq ->
  m [estimate]
runSearchPhaseSync phase bapCtx bppCtx searchReq = do
  searchRes <- phase.spBapSearch bapCtx searchReq
  bppSearchIn <- runLinkSync phase.spOutboundLink searchRes
  bppSearchRes <- phase.spBppSearch bppCtx bppSearchIn
  onSearchIn <- runLinkSync phase.spCallbackLink bppSearchRes
  phase.spBapOnSearch onSearchIn

-- | Run select phase: BAP select → ACL → BPP select → ACL → BAP onSelect
runSelectPhaseSync ::
  (Monad m) =>
  SelectPhase m bapCtx estimateId selectReq selectRes selectWire
    bppCtx bppSelectIn
    onSelectWire onSelectIn quote ->
  bapCtx ->
  bppCtx ->
  estimateId ->
  selectReq ->
  m quote
runSelectPhaseSync phase bapCtx bppCtx estimateId selectReq = do
  selectRes <- phase.slBapSelect bapCtx estimateId selectReq
  bppSelectIn <- runLinkSync phase.slOutboundLink selectRes
  phase.slBppSelect bppCtx bppSelectIn
  -- on_select comes back (driver offers etc)
  onSelectIn <- runLinkSync phase.slCallbackLink selectRes
  phase.slBapOnSelect onSelectIn

-- | Run confirm phase: BAP confirm → ACL → BPP confirm → ACL → BAP onConfirm
runConfirmPhaseSync ::
  (Monad m) =>
  ConfirmPhase m bapCtx quoteId confirmRes confirmWire
    bppCtx bppConfirmIn bppConfirmRes
    onConfirmWire onConfirmIn booking ride ->
  bapCtx ->
  bppCtx ->
  quoteId ->
  m (booking, ride)
runConfirmPhaseSync phase bapCtx bppCtx quoteId = do
  (booking, confirmRes) <- phase.cfBapConfirm bapCtx quoteId
  bppConfirmIn <- runLinkSync phase.cfOutboundLink confirmRes
  bppConfirmRes <- phase.cfBppConfirm bppCtx bppConfirmIn
  onConfirmIn <- runLinkSync phase.cfCallbackLink bppConfirmRes
  ride <- phase.cfBapOnConfirm booking onConfirmIn
  pure (booking, ride)

-- | Run start ride: BPP startRide → ACL → BAP onStatus
runStartRideSync ::
  (Monad m) =>
  RideExecPhase m bppCtx rideId startReq startRes
    endReq endRes fareBreakup
    statusWire statusIn
    booking ride ->
  bppCtx ->
  booking ->
  ride ->
  rideId ->
  startReq ->
  m ()
runStartRideSync phase bppCtx booking ride rideId startReq = do
  startRes <- phase.reBppStartRide bppCtx rideId startReq
  statusIn <- runLinkSync phase.reStartStatusLink startRes
  phase.reBapOnStart booking ride statusIn

-- | Run end ride: BPP endRide → ACL → BAP onRideEnd
runEndRideSync ::
  (Monad m) =>
  RideExecPhase m bppCtx rideId startReq startRes
    endReq endRes fareBreakup
    statusWire statusIn
    booking ride ->
  bppCtx ->
  booking ->
  ride ->
  rideId ->
  endReq ->
  m fareBreakup
runEndRideSync phase bppCtx booking ride rideId endReq = do
  endRes <- phase.reBppEndRide bppCtx rideId endReq
  statusIn <- runLinkSync phase.reEndStatusLink endRes
  phase.reBapOnEnd booking ride statusIn

-- ---------------------------------------------------------------------------
-- Full flow result
-- ---------------------------------------------------------------------------

data FlowResult estimate quote booking ride fareBreakup = FlowResult
  { frEstimates :: [estimate],
    frQuote :: quote,
    frBooking :: booking,
    frRide :: ride,
    frFareBreakup :: fareBreakup
  }

-- ---------------------------------------------------------------------------
-- Full flow runner (chains all phases with decisions)
-- ---------------------------------------------------------------------------

-- | Run the entire ride-hailing flow synchronously.
--
-- Chains all phases, pausing at decision points for FlowDecisions callbacks.
-- Both BAP and BPP handlers run in the same monad.
--
-- Flow:
--   search → on_search → ⏸️ riderPickEstimate
--   → select → on_select → ⏸️ riderConfirmQuote
--   → confirm → on_confirm → ⏸️ driverStartRide
--   → startRide → ⏸️ driverEndRide
--   → endRide → DONE
--
-- Note: init/on_init phase is folded into the confirm step for now.
-- It will be separated when we wire the real handlers.
runFlowSync ::
  (Monad m) =>
  -- Flow phases
  SearchPhase m bapCtx searchReq searchRes searchWire bppCtx bppSearchIn bppSearchRes onSearchWire onSearchIn estimate ->
  SelectPhase m bapCtx estimateId selectReq selectRes selectWire bppCtx bppSelectIn onSelectWire onSelectIn quote ->
  ConfirmPhase m bapCtx quoteId confirmRes confirmWire bppCtx bppConfirmIn bppConfirmRes onConfirmWire onConfirmIn booking ride ->
  RideExecPhase m bppCtx rideId startRideReq startRes endRideReq endRes fareBreakup statusWire statusIn booking ride ->
  -- Contexts
  bapCtx ->
  bppCtx ->
  -- Input
  searchReq ->
  -- External actor decisions
  FlowDecisions m estimate selectReq quote quoteId booking ride startRideReq endRideReq ->
  -- How to extract IDs from domain objects (app provides these)
  (estimate -> estimateId) ->
  (ride -> rideId) ->
  -- Result
  m (FlowResult estimate quote booking ride fareBreakup)
runFlowSync searchP selectP confirmP rideExecP bapCtx bppCtx searchReq
  (FlowDecisions pickEst confirmQ startR endR payConfirm extCbs)
  getEstimateId getRideId = do
  -- Phase 1: Search
  estimates <- runSearchPhaseSync searchP bapCtx bppCtx searchReq

  -- ⏸️ Rider picks estimate
  (chosenEstimate, selReq) <- pickEst estimates

  -- Phase 2: Select
  quote <- runSelectPhaseSync selectP bapCtx bppCtx (getEstimateId chosenEstimate) selReq

  -- ⏸️ Rider confirms
  confReq <- confirmQ quote

  -- Phase 3: Confirm (includes init/on_init internally)
  (booking, ride) <- runConfirmPhaseSync confirmP bapCtx bppCtx confReq

  -- ⏸️ Optional: payment confirmation
  case payConfirm of
    Just paymentCb -> paymentCb booking
    Nothing -> pure ()

  -- ⏸️ Driver starts ride
  stReq <- startR booking ride

  -- Phase 4: Start ride
  runStartRideSync rideExecP bppCtx booking ride (getRideId ride) stReq

  -- ⏸️ Optional: external callbacks during ride (insurance, safety, etc.)
  mapM_ (\cb -> cb booking ride) extCbs

  -- ⏸️ Driver ends ride
  enReq <- endR booking ride

  -- Phase 5: End ride
  fareBreakup <- runEndRideSync rideExecP bppCtx booking ride (getRideId ride) enReq

  pure
    FlowResult
      { frEstimates = estimates,
        frQuote = quote,
        frBooking = booking,
        frRide = ride,
        frFareBreakup = fareBreakup
      }
