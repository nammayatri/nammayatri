{-
  Ride-hailing flow: state machine + phase definitions with PhaseLinks.

  Each Beckn action pair (e.g., search + on_search) is:
    BAP handler → PhaseLink (ACL) → BPP handler → PhaseLink (ACL) → BAP callback handler

  The flow record bundles all phases. Each handler is parameterized by real
  domain types from the existing codebase. PhaseLinks carry the ACL conversion.

  When wired in the app:
    - Handlers point to existing Domain.Action functions
    - PhaseLinks point to existing Beckn.ACL functions
    - Context is populated from the request
-}
module MobilityFlow.Flows.RideHailing
  ( -- * State machine
    RideFlowState (..),
    rideFlowTransitions,

    -- * Flow definition
    RideHailingFlow (..),
    SearchPhase (..),
    SelectPhase (..),
    InitPhase (..),
    ConfirmPhase (..),
    RideExecPhase (..),
    CancelPhase (..),
  )
where

import Kernel.Prelude
import MobilityFlow.Core.PhaseLink (PhaseLink)
import MobilityFlow.Core.StateMachine

-- ---------------------------------------------------------------------------
-- State machine
-- ---------------------------------------------------------------------------

data RideFlowState
  = Idle
  | Searching
  | Quoted
  | Selected
  | Initialized
  | Booked
  | Assigned
  | InProgress
  | Completed
  | Cancelled
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

rideFlowTransitions :: [FlowTransition RideFlowState]
rideFlowTransitions =
  [ -- Discovery
    FlowTransition [Idle] (RiderAPI "search") Searching "discover",
    FlowTransition [Searching] (BecknCallback "on_search") Quoted "receiveEstimates",
    -- Selection
    FlowTransition [Quoted] (RiderAPI "select") Selected "selectQuote",
    FlowTransition [Selected] (BecknCallback "on_select") Quoted "receiveQuote",
    FlowTransition [Quoted] (RiderAPI "select") Selected "reselectQuote",
    -- Init
    FlowTransition [Quoted] (RiderAPI "init") Initialized "initBooking",
    FlowTransition [Initialized] (BecknCallback "on_init") Quoted "initConfirmed",
    -- Booking
    FlowTransition [Quoted] (RiderAPI "confirm") Booked "commitBooking",
    FlowTransition [Booked] (BecknCallback "on_confirm") Assigned "driverAssigned",
    -- Ride execution
    FlowTransition [Assigned] (DriverAPI "startRide") InProgress "rideStarted",
    FlowTransition [InProgress] (DriverAPI "endRide") Completed "rideEnded",
    -- Status updates
    FlowTransition [Assigned] (BecknCallback "on_status") Assigned "statusUpdate",
    FlowTransition [InProgress] (BecknCallback "on_status") InProgress "statusUpdate",
    -- Cancellation
    FlowTransition [Searching, Quoted, Selected, Initialized] (RiderAPI "cancel") Cancelled "riderCancelPre",
    FlowTransition [Booked, Assigned] (RiderAPI "cancel") Cancelled "riderCancelPost",
    FlowTransition [Assigned, InProgress] (DriverAPI "cancel") Cancelled "driverCancel"
  ]

-- ---------------------------------------------------------------------------
-- Phase definitions
-- ---------------------------------------------------------------------------
-- Each phase bundles: handler + outbound ACL link + handler + return ACL link
-- The type params are the ACTUAL domain types from the existing codebase.

-- | Search phase: rider searches → BAP sends to BPP → BPP returns estimates
--
-- Real types when wired:
--   bapCtx           = BAPContext Person Client
--   searchReq        = SharedLogic.Search.SearchReq (OneWaySearch | RentalSearch | ...)
--   searchRes        = Domain.Action.UI.Search.SearchRes
--   searchWire       = BecknV2 Spec.SearchReqV2
--   bppCtx           = BPPContext Merchant MerchantOperatingCity
--   bppSearchIn      = (Domain.Action.Beckn.Search.ValidatedDSearchReq, DSearchReq)
--   bppSearchRes     = Domain.Action.Beckn.Search.DSearchRes
--   onSearchWire     = BecknV2 Spec.OnSearchReqV2
--   onSearchIn       = Domain.Action.Beckn.OnSearch.ValidatedOnSearchReq
--   estimate         = Domain.Types.Estimate
data SearchPhase m bapCtx searchReq searchRes searchWire
                   bppCtx bppSearchIn bppSearchRes
                   onSearchWire onSearchIn estimate = SearchPhase
  { -- BAP: rider initiates search
    spBapSearch :: bapCtx -> searchReq -> m searchRes,
    -- ACL: BAP domain → Beckn wire
    spOutboundLink :: PhaseLink m searchRes searchWire bppSearchIn,
    -- BPP: processes search, finds drivers
    spBppSearch :: bppCtx -> bppSearchIn -> m bppSearchRes,
    -- ACL: BPP domain → Beckn callback wire
    spCallbackLink :: PhaseLink m bppSearchRes onSearchWire onSearchIn,
    -- BAP: receives estimates from callback
    spBapOnSearch :: onSearchIn -> m [estimate]
  }

-- | Select phase: rider picks an estimate → BPP confirms quote
data SelectPhase m bapCtx estimateId selectReq selectRes selectWire
                   bppCtx bppSelectIn
                   onSelectWire onSelectIn quote = SelectPhase
  { slBapSelect :: bapCtx -> estimateId -> selectReq -> m selectRes,
    slOutboundLink :: PhaseLink m selectRes selectWire bppSelectIn,
    slBppSelect :: bppCtx -> bppSelectIn -> m (), -- BPP select is fire-and-forget (offers to drivers)
    slCallbackLink :: PhaseLink m selectRes onSelectWire onSelectIn, -- on_select comes from driver offer acceptance
    slBapOnSelect :: onSelectIn -> m quote
  }

-- | Init phase: BAP sends init to BPP → BPP returns payment/booking details
data InitPhase m bapCtx initReq initRes initWire
                 bppCtx bppInitIn bppInitRes
                 onInitWire onInitIn = InitPhase
  { inBapInit :: bapCtx -> initReq -> m initRes,
    inOutboundLink :: PhaseLink m initRes initWire bppInitIn,
    inBppInit :: bppCtx -> bppInitIn -> m bppInitRes,
    inCallbackLink :: PhaseLink m bppInitRes onInitWire onInitIn,
    inBapOnInit :: onInitIn -> m ()
  }

-- | Confirm phase: rider confirms → BPP assigns driver → ride created
data ConfirmPhase m bapCtx quoteId confirmRes confirmWire
                    bppCtx bppConfirmIn bppConfirmRes
                    onConfirmWire onConfirmIn booking ride = ConfirmPhase
  { cfBapConfirm :: bapCtx -> quoteId -> m (booking, confirmRes),
    cfOutboundLink :: PhaseLink m confirmRes confirmWire bppConfirmIn,
    cfBppConfirm :: bppCtx -> bppConfirmIn -> m bppConfirmRes,
    cfCallbackLink :: PhaseLink m bppConfirmRes onConfirmWire onConfirmIn,
    cfBapOnConfirm :: booking -> onConfirmIn -> m ride
  }

-- | Ride execution: startRide → endRide (both triggered by driver)
data RideExecPhase m bppCtx rideId startReq startRes
                     endReq endRes fareBreakup
                     statusWire statusIn
                     booking ride = RideExecPhase
  { reBppStartRide :: bppCtx -> rideId -> startReq -> m startRes,
    reStartStatusLink :: PhaseLink m startRes statusWire statusIn,
    reBapOnStart :: booking -> ride -> statusIn -> m (),
    reBppEndRide :: bppCtx -> rideId -> endReq -> m endRes,
    reEndStatusLink :: PhaseLink m endRes statusWire statusIn,
    reBapOnEnd :: booking -> ride -> statusIn -> m fareBreakup
  }

-- | Cancel phase: either side can cancel
data CancelPhase m bapCtx booking ride cancelReq cancelRes cancelWire
                   bppCtx bppCancelIn = CancelPhase
  { caBapCancel :: bapCtx -> booking -> Maybe ride -> cancelReq -> m cancelRes,
    caOutboundLink :: PhaseLink m cancelRes cancelWire bppCancelIn,
    caBppCancel :: bppCtx -> bppCancelIn -> m ()
  }

-- ---------------------------------------------------------------------------
-- Full flow: composition of all phases
-- ---------------------------------------------------------------------------

-- | The complete ride-hailing flow.
-- Each phase is self-contained with its own type params.
-- The flow is the composition of all phases.
--
-- In practice, the app wires this once and runFlowSync chains all phases.
data RideHailingFlow m searchP selectP initP confirmP rideExecP cancelP = RideHailingFlow
  { rideSearch :: searchP,
    rideSelect :: selectP,
    rideInit :: initP,
    rideConfirm :: confirmP,
    rideExec :: rideExecP,
    rideCancel :: cancelP
  }
