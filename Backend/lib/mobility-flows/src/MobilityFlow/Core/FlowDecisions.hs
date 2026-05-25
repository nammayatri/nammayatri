{-
  FlowDecisions: external actor inputs at pause points in the flow.

  The ride flow is NOT a continuous pipeline. It pauses at decision points
  where external actors (rider, driver, third-party services) provide input:

    search → on_search → ⏸️ RIDER picks estimate
                       → select → on_select → ⏸️ RIDER confirms
                       → init → on_init → confirm → on_confirm → ⏸️ DRIVER starts ride
                       → startRide → ⏸️ DRIVER ends ride
                       → endRide → DONE

  In production (async): these pauses are natural — the system stores state
  and waits for the next API call. The "decision" IS the API call.

  In sync mode (testing): FlowDecisions provides callbacks that simulate
  rider/driver/third-party choices. Tests control what happens at each pause.

  FlowDecisions is parameterized by the same domain types as the flow,
  so it works with real Estimate, Quote, Booking, Ride, etc.
-}
module MobilityFlow.Core.FlowDecisions
  ( FlowDecisions (..),
  )
where

import Kernel.Prelude

-- | External actor decisions at each pause point.
--
-- Parameterized by actual domain types from the existing codebase.
-- When wired in hunit-tests:
--   FlowDecisions AppM Estimate SelectReq Quote ConfirmReq Booking Ride StartRideReq EndRideReq
--
-- In tests, provide deterministic callbacks:
--   riderPickEstimate = \estimates -> pure (head estimates, defaultSelectReq)
--
-- In production, these don't exist — each decision is a separate API call.
data FlowDecisions m estimate selectReq quote confirmReq booking ride startRideReq endRideReq = FlowDecisions
  { -- | Rider sees estimates, picks one and provides select params.
    -- In production: rider calls POST /select with estimateId + selectReq
    -- In test: callback picks from the list
    riderPickEstimate :: [estimate] -> m (estimate, selectReq),
    -- | Rider sees confirmed quote, decides to confirm.
    -- In production: rider calls POST /confirm with quoteId + payment params
    -- In test: callback approves the quote
    riderConfirmQuote :: quote -> m confirmReq,
    -- | Driver gets ride offer, decides to start.
    -- In production: driver calls POST /ride/{id}/start with OTP
    -- In test: callback provides start ride params
    driverStartRide :: booking -> ride -> m startRideReq,
    -- | Driver completes trip, decides to end.
    -- In production: driver calls POST /ride/{id}/end with location
    -- In test: callback provides end ride params
    driverEndRide :: booking -> ride -> m endRideReq,
    -- | Optional: third-party payment confirmation (between init and confirm).
    -- In production: payment gateway webhook
    -- In test: callback confirms payment
    -- Nothing means no payment step required.
    paymentConfirmation :: Maybe (booking -> m ()),
    -- | Optional: any other external callback needed before proceeding.
    -- Extensible: add fields here for insurance, KYC, etc.
    externalCallbacks :: [booking -> ride -> m ()]
  }
