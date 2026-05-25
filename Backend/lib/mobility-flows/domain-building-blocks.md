# Domain Building Blocks — Architecture Proposal

## Problem Statement

When building new features (e.g., add stop, edit destination) or new ride types, developers must:
- Figure out what to reuse vs write from scratch
- Understand existing code across ~12 files and 4 layers to know where to plug in
- Replicate implicit patterns that live in developers' heads, not in code
- Touch types, domain actions, Beckn ACL, notifications on both rider and driver sides
- Repeat the same 10 mechanical steps that every modification shares

The goal: **building blocks are concrete shared implementations of invariant steps.** When AddStop exists and you're building EditDestination, you only write what's unique to EditDestination. State validation, lock acquisition, BookingUpdateRequest creation, Beckn UPDATE envelope, driver prompt, callback routing — all done once in the framework.

---

## Key Design Principles

1. **Building blocks = shared implementation, not just shared interfaces.** A typeclass that unifies the shape is not enough. The common code must be written once and actually execute for all modifications.

2. **Flows are event-driven state machines, not synchronous pipelines.** A ride-hailing flow involves 4 participants (rider app, BAP server, BPP server, driver app), multiple async API calls, and arbitrary waits for human input. There is no single process running `discover >> price >> select >> commit`.

3. **Developer writes only what varies.** The framework handles everything that's the same across all modifications/phases. The developer provides a small "recipe" of the unique business logic.

---

## Current Architecture — What Exists

### The Good (Implicit Building Blocks)

- **Beckn lifecycle** gives a universal flow skeleton: `search → on_search → select → on_select → init → on_init → confirm → on_confirm`
- **SharedLogic/** centralizes reusable business logic (Search, Confirm, Cancel, Quote, FareCalculator)
- **ADTs** (`BookingDetails`, `QuoteDetails`, `SearchReq`, `TripCategory`) give type-safe branching per ride type
- **NammaDSL** handles storage/API boilerplate generation
- **Mid-ride modifications** follow a recognizable pattern: validate → Beckn transform → send → handle → notify

### The Problems (Blocks are Implicit, Not Composable)

1. **Flow knowledge is implicit.** There's no single place that says "this is the add-stop flow." A developer must read through `Domain/Action/UI/Booking.hs` (rider), `Beckn/ACL/Update.hs` (both sides), `Domain/Action/Beckn/Update.hs` (driver), notification code, etc.

2. **Ride type branching is scattered.** `TripCategory` branches via pattern matching in `Search.hs`, `Quote.hs`, `Booking.hs`, `Confirm.hs`, `FareCalculator`, and many more. Adding a new ride type means hunting through dozens of files.

3. **Mid-ride modifications reinvent the pattern.** Add-stop, edit-destination, change-service-tier, add-baggage all share 10 identical mechanical steps but each implements them from scratch (~500 lines per modification, ~200 of which are copy-paste).

4. **Beckn ACL is mechanical but hand-written.** Every flow has a builder (domain → Beckn spec) and a parser (Beckn spec → domain). This is ~20-30% of code and follows a predictable structure.

---

## Evidence: What's Actually Shared vs Unique in Modifications

Based on detailed code analysis of AddStop, EditDestination, ChangeServiceTier, and AddBaggage:

### Steps That Are IDENTICAL Across All 4 Modifications (The Framework)

| # | Step | Current Code (repeated 4x) |
|---|------|----------------------------|
| 1 | Fetch booking from DB | `QRB.findById bookingId >>= fromMaybeM` |
| 2 | Fetch merchant config | `CQMerchant.findById merchantId >>= fromMaybeM` |
| 3 | Get BPP booking ID | `booking.bppBookingId & fromMaybeM` |
| 4 | Generate message ID | `generateGUID` |
| 5 | Build `UpdateBuildReq` shell | `UpdateBuildReq { bppId, bppUrl, transactionId, messageId, city, details }` |
| 6 | Build Beckn UPDATE message | `ACL.buildUpdateReq dUpdateReq` |
| 7 | Send to BPP via HTTP | `CallBPP.updateV2 booking.providerUrl becknUpdateReq` |
| 8 | Return APISuccess to caller | `pure APISuccess` |
| 9 | (BPP) Parse event type from Beckn | Pattern match on `fulfillmentStateDescriptor.code` |
| 10 | (BPP) Fetch booking, validate exists | `QRB.findById bookingId >>= fromMaybeM` |

**That's ~200 lines of code repeated identically in every modification.**

### Steps That VARY Per Modification (The Recipe)

| Step | AddStop | EditDestination | ChangeServiceTier | AddBaggage |
|------|---------|-----------------|-------------------|------------|
| Valid states | TRIP_ASSIGNED, INPROGRESS | INPROGRESS | NEW, CONFIRMED | NEW, CONFIRMED |
| Valid trip types | Rental only | Any | RideOTP only | RideOTP only |
| Request data | Location (gps + address) | Location (destination) | QuoteId | numberOfLuggages |
| BAP business logic | Create Location + LocationMapping, update booking.stopLocationId | Send with SOFT_UPDATE status | Look up quote, extract tier | Just pass luggage count |
| BPP business logic | Create Location + LocationMapping, update booking, notify driver | Calculate new route + fare, create BookingUpdateRequest | Look up quote fare, update booking tier + fare | Recalculate fare with luggage, update booking |
| Driver approval needed? | No | Yes (soft update → driver accept → confirm) | No | No |
| Fare recalculation? | No | Yes (OSRM route + FareCalculator) | No (use quote's pre-calculated fare) | Yes (re-run FareCalculator with luggage) |
| Beckn payload | Fulfillment stops | Fulfillment stops + rideId + status | Item with quoteId + tags with tier | Tags with luggage count |
| on_update handling | N/A (notification only) | Create LocationMappings, update fare/distance | Update vehicleServiceTier + fare | Update estimatedFare + breakups |

### Synchronization Models

| Model | Modifications | How it works |
|-------|--------------|--------------|
| **Immediate** | AddStop | BAP→BPP UPDATE, BPP processes + notifies driver. No callback needed. |
| **Fire-and-forget** | ChangeServiceTier, AddBaggage | BAP→BPP UPDATE, BPP processes + sends ON_UPDATE callback, BAP applies changes. |
| **Two-phase with driver approval** | EditDestination | BAP→BPP UPDATE (SOFT), BPP calculates + shows to driver, Driver accepts/rejects, BPP→BAP ON_UPDATE (CONFIRM/REJECT). |

---

## Domain Vocabulary

### Nouns (Entities)

| Building Block | What it represents | Current type |
|---|---|---|
| `Intent` | Rider's desire to travel | `SearchRequest` |
| `Offer` | Priced option from supply side | `Estimate` / `Quote` / `FRFSQuote` |
| `Commitment` | Agreed-upon trip | `Booking` / `FRFSTicketBooking` |
| `Fulfillment` | The actual journey | `Ride` / `FRFSTicket` |
| `Participant` | Any actor | `Person` (rider/driver) |
| `Vehicle` | Transport unit | `Vehicle` / service tier |
| `Location` | A point in space | `Location` + `LocationMapping` |
| `Route` | Path between locations | Route polyline + waypoints |
| `Fare` | Price computation | `FareBreakup` / `FarePolicy` |
| `Settlement` | Financial resolution | `Payment` / `PaymentOrder` |
| `Trust` | Quality signal | `Rating` / `Feedback` / driver score |
| `Modification` | Change to active flow | `BookingUpdateRequest` / stop add/edit |

### Verbs (Actions)

| Verb | Phase | Current implementation |
|---|---|---|
| `Discover` | Pre-booking | `Search.search` |
| `Price` | Pre-booking | `FareCalculator`, `Estimate` creation |
| `Select` | Pre-booking | `Select.select` |
| `Commit` | Booking | `Confirm.confirm` → `SConfirm.confirm` |
| `Match` | Assignment | Driver offer / allocation |
| `Fulfill` | In-ride | `StartRide`, `EndRide`, tracking |
| `Modify` | In-ride | Add stop, edit dest, change tier |
| `Settle` | Post-ride | Payment processing |
| `Evaluate` | Post-ride | Rating, feedback |
| `Abort` | Any phase | `Cancel` (with phase-specific logic) |
| `Notify` | Cross-cutting | FCM, SMS, in-app |

### Axes of Variation (Policies)

| Axis | Examples | Role |
|---|---|---|
| `TripType` | OneWay, Rental, InterCity, Pool, Scheduled, Delivery, Ambulance, PublicTransit | Determines which phases are active and how each behaves |
| `PricingPolicy` | Distance-based, time-based, package, surge, flat-fare, negotiated | Plugged into `Price` verb |
| `MatchingPolicy` | Nearest, auction/bid, scheduled, manual, queue-based, pool | Plugged into `Match` verb |
| `PaymentPolicy` | Prepaid, postpaid, cash, wallet, corporate, subscription | Plugged into `Settle` verb |
| `FulfillmentPolicy` | Real-time tracked, QR ticket, scheduled pickup | Plugged into `Fulfill` verb |

---

## Proposed Architecture

### Fundamental Insight: Flows are Event-Driven State Machines

A ride-hailing flow is NOT a synchronous pipeline. It involves:

```
Rider App                    BAP Server              BPP Server              Driver App
    |                            |                        |                      |
    |-- POST /search ---------->|                        |                      |
    |                            |-- Beckn search ------>|                      |
    |                            |                        |                      |
    |                            |<-- Beckn on_search ---|                      |
    |<-- GET /estimates --------|                        |                      |
    |                            |                        |                      |
    |-- POST /select ---------->|                        |                      |
    |                            |-- Beckn select ------>|                      |
    |                            |                        |-- show to driver -->|
    |                            |                        |                      |
    |                            |                        |<-- driver accepts --|
    |                            |<-- Beckn on_confirm --|                      |
    |<-- booking confirmed -----|                        |                      |
```

Each "phase" is triggered by a different event (rider API call, Beckn callback, driver API call, system timer). Arbitrary time passes between phases. Different phases run on different servers.

Therefore: **the flow definition is a state machine spec, not a pipeline.** Each building block is an **event handler**, not a step in a sequence.

### Layer 1: Flow as a State Machine

```haskell
-- What can trigger a phase transition
data Trigger
  = RiderAPI Text              -- rider app calls an endpoint
  | DriverAPI Text             -- driver app calls an endpoint
  | BecknCallback BecknAction  -- BPP/BAP sends a Beckn callback
  | SystemEvent Text           -- timer, scheduler, etc.

-- The flow definition: a routing table of trigger → handler
rideHailingFlow :: FlowMachine
rideHailingFlow = flowMachine
  [ on (RiderAPI "search")           $ discover    -- → state: Searching
  , on (BecknCallback "on_search")   $ price       -- → state: Quoted
  , on (RiderAPI "select")           $ select      -- → state: Quoted (can re-select)
  , on (RiderAPI "confirm")          $ commit      -- → state: Booked
  , on (BecknCallback "on_confirm")  $ assigned    -- → state: Assigned
  , on (DriverAPI "startRide")       $ startRide   -- → state: InProgress
  , on (DriverAPI "endRide")         $ complete    -- → state: Completed
  , on (SystemEvent "payment")       $ settle      -- → state: Settled
  , on (RiderAPI "feedback")         $ evaluate    -- → state: Evaluated
  -- Cancel from multiple states:
  , on (RiderAPI "cancel")           $ abort       -- → state: Cancelled
  , on (DriverAPI "cancel")          $ abort       -- → state: Cancelled
  ]

frfsFlow :: FlowMachine
frfsFlow = flowMachine
  [ on (RiderAPI "search")           $ discover
  , on (BecknCallback "on_search")   $ showCatalog
  , on (RiderAPI "select")           $ selectTicket
  , on (BecknCallback "on_init")     $ preparePayment
  , on (RiderAPI "confirm")          $ issueTicket
  , on (BecknCallback "on_confirm")  $ activateTicket
  , on (RiderAPI "cancel")           $ cancelTicket
  ]
```

Each entry is an independent event handler. The framework:
- Validates the current state allows this transition
- Calls the handler
- Updates the entity state
- The handler returns, and the system waits for the next event (which may come seconds or hours later)

### Layer 2: Modifications as Concrete Shared Framework

This is where the biggest win is. The framework is NOT just an interface — it's a **concrete implementation of the 10 common steps** with hooks for the unique parts.

```haskell
-- The "recipe" a developer provides. ONLY the unique parts.
data ModificationRecipe m = ModificationRecipe
  { modName          :: Text                    -- "ADD_STOP", "EDIT_LOCATION", etc.

    -- Declarative metadata
  , validStates      :: [BookingStatus]         -- when can this trigger?
  , validTripTypes   :: Maybe [TripCategory]    -- Nothing = any trip type
  , syncModel        :: SyncModel               -- how BAP↔BPP coordinate

    -- BAP side: what to do when rider initiates (the UNIQUE logic)
  , onBAPInitiate    :: Booking -> req -> m BecknPayloadDetails

    -- BPP side: what to do when Beckn UPDATE arrives (the UNIQUE logic)
  , onBPPReceive     :: Booking -> Maybe Ride -> BecknPayloadDetails -> m BPPOutcome

    -- BAP side: what to do when ON_UPDATE callback arrives (the UNIQUE logic)
    -- Nothing for Immediate sync model (no callback expected)
  , onBAPCallback    :: Maybe (Booking -> OnUpdatePayload -> m ())
  }

data SyncModel
  = Immediate              -- BPP processes, notifies driver, no callback
  | FireAndForget          -- BPP processes, sends ON_UPDATE, BAP applies
  | WithDriverApproval     -- BPP calculates, shows to driver, driver responds
      { driverPrompt :: DriverPromptConfig }

data BPPOutcome
  = Done                                  -- Immediate: nothing more to do
  | SendCallback OnUpdatePayload          -- FireAndForget: send back to BAP
  | ShowToDriver DriverPromptData         -- WithDriverApproval: wait for driver
      { onAccept :: m OnUpdatePayload     -- what to do if driver accepts
      , onReject :: m OnUpdatePayload     -- what to do if driver rejects
      }

-- What varies in the Beckn UPDATE message (the only part the developer fills in)
data BecknPayloadDetails = BecknPayloadDetails
  { descriptorCode    :: Text              -- "ADD_STOP", "EDIT_LOCATION", etc.
  , fulfillmentStops  :: Maybe [Stop]      -- for location-based mods
  , fulfillmentId     :: Maybe Text        -- bppRideId if needed
  , tags              :: Maybe [Tag]       -- for data-carrying mods
  , items             :: Maybe [Item]      -- for quote-based mods
  , updateTarget      :: Text              -- "order.fullfillments" etc.
  }
```

### The Framework: Concrete Implementation (Written Once)

```haskell
-- This function IS the building block. It handles ALL the common steps.
-- Every modification calls this with its recipe. The common code runs once.

runModification :: ModificationRecipe m -> ModificationRequest -> m APISuccess
runModification recipe rawReq = do

  -- ===== COMMON STEP 1-4: Fetch & validate (same for ALL modifications) =====
  booking  <- QRB.findById rawReq.bookingId >>= fromMaybeM (BookingNotFound rawReq.bookingId)
  merchant <- CQMerchant.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId)

  -- Validate booking state (declarative, from recipe)
  unless (booking.status `elem` recipe.validStates) $
    throwError (BookingInvalidStatus booking.status)

  -- Validate trip type if recipe restricts it
  forM_ recipe.validTripTypes $ \allowedTypes ->
    unless (booking.tripCategory `elem` allowedTypes) $
      throwError (InvalidTripType booking.tripCategory)

  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")

  -- ===== UNIQUE STEP: Run modification-specific BAP logic =====
  payloadDetails <- recipe.onBAPInitiate booking rawReq.data

  -- ===== COMMON STEP 5-8: Build & send Beckn UPDATE (same for ALL) =====
  msgId <- generateGUID
  let updateReq = UpdateBuildReq
        { bppId          = booking.providerId
        , bppUrl         = booking.providerUrl
        , transactionId  = booking.transactionId
        , messageId      = msgId
        , city           = merchant.city
        , details        = payloadDetails
        }
  becknMsg <- ACL.buildUpdateReq updateReq
  CallBPP.updateV2 booking.providerUrl becknMsg

  pure APISuccess


-- BPP side: framework handles Beckn parsing and routing
handleBPPUpdate :: Map Text (ModificationRecipe m) -> BecknUpdateReq -> m ()
handleBPPUpdate registry becknReq = do

  -- ===== COMMON: Parse event type, extract booking ID =====
  eventType <- extractEventType becknReq
  bookingId <- extractBookingId becknReq

  -- ===== COMMON: Look up which recipe handles this event type =====
  recipe <- Map.lookup eventType registry
    & fromMaybeM (UnknownModificationType eventType)

  -- ===== COMMON: Fetch booking, validate =====
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId)
  ride    <- QRide.findActiveByRBId bookingId  -- Maybe Ride

  payloadDetails <- extractPayloadDetails becknReq

  -- ===== UNIQUE: Run modification-specific BPP logic =====
  outcome <- recipe.onBPPReceive booking ride payloadDetails

  -- ===== COMMON: Handle outcome based on sync model =====
  case outcome of
    Done -> pure ()

    SendCallback onUpdatePayload -> do
      -- Common: build ON_UPDATE, send back to BAP
      CallBAP.sendOnUpdate booking onUpdatePayload

    ShowToDriver promptData -> do
      -- Common: create BookingUpdateRequest (if needed)
      updateReqId <- createBookingUpdateRequest booking recipe.modName promptData
      -- Common: send FCM overlay to driver with accept/reject
      sendDriverPrompt ride.driverId updateReqId promptData
      -- Common: register handler for driver's response
      -- (When driver hits accept/reject API, framework calls promptData.onAccept or .onReject,
      --  then sends ON_UPDATE back to BAP)


-- BPP side: framework handles driver accept/reject
handleDriverResponse :: Map Text (ModificationRecipe m) -> DriverResponseReq -> m ()
handleDriverResponse registry driverReq = do

  -- ===== COMMON: Fetch update request, validate not expired =====
  updateReq <- QBUR.findById driverReq.updateReqId >>= fromMaybeM NotFound
  unless (updateReq.status == USER_CONFIRMED) $ throwError InvalidStatus
  unless (updateReq.validTill > now) $ throwError Expired

  -- ===== COMMON: Acquire Redis lock =====
  locked <- Redis.tryLockRedis (modLockKey updateReq.rideId) 20
  unless locked $ throwError ConcurrentModification

  booking <- QRB.findById updateReq.bookingId >>= fromMaybeM NotFound
  ride    <- QRide.findById updateReq.rideId >>= fromMaybeM NotFound
  recipe  <- Map.lookup updateReq.modType registry & fromMaybeM NotFound

  case driverReq.action of
    Accept -> do
      -- ===== COMMON: Update status =====
      QBUR.updateStatusById DRIVER_ACCEPTED updateReq.id
      -- ===== UNIQUE: Run accept logic =====
      case recipe.syncModel of
        WithDriverApproval {..} -> do
          onUpdatePayload <- onAccept
          CallBAP.sendOnUpdate booking onUpdatePayload
        _ -> pure ()

    Reject -> do
      QBUR.updateStatusById DRIVER_REJECTED updateReq.id
      case recipe.syncModel of
        WithDriverApproval {..} -> do
          onUpdatePayload <- onReject
          CallBAP.sendOnUpdateError booking onUpdatePayload
        _ -> pure ()

  Redis.unlockRedis (modLockKey updateReq.rideId)


-- BAP side: framework handles ON_UPDATE callbacks
handleBAPOnUpdate :: Map Text (ModificationRecipe m) -> BecknOnUpdateReq -> m ()
handleBAPOnUpdate registry becknReq = do

  -- ===== COMMON: Parse event type, extract booking =====
  eventType <- extractEventType becknReq
  bookingId <- extractBookingId becknReq
  recipe    <- Map.lookup eventType registry & fromMaybeM NotFound
  booking   <- QRB.findById bookingId >>= fromMaybeM NotFound

  -- ===== UNIQUE: Run modification-specific callback logic =====
  case recipe.onBAPCallback of
    Just handler -> handler booking (extractOnUpdatePayload becknReq)
    Nothing      -> pure ()  -- Immediate sync model, no callback expected
```

### What "AddStop" Looks Like With the Framework

```haskell
-- THIS IS ALL A DEVELOPER WRITES. ~40 lines. No Beckn boilerplate.
-- No state validation. No lock management. No callback routing.

addStopRecipe :: ModificationRecipe m
addStopRecipe = ModificationRecipe
  { modName        = "ADD_STOP"
  , validStates    = [TRIP_ASSIGNED, CONFIRMED]
  , validTripTypes = Just [Rental OneWayMode]
  , syncModel      = Immediate

  , onBAPInitiate = \booking req -> do
      -- Unique to AddStop: create location + mapping
      location <- buildLocation req.gps req.address
      locationMapping <- buildLocationMapping location.id booking.id
      QL.create location
      QLM.create locationMapping
      QRB.updateStop booking (Just location) (Just True)
      -- Return what goes in the Beckn UPDATE message
      pure $ BecknPayloadDetails
        { descriptorCode   = "ADD_STOP"
        , fulfillmentStops = Just [mkStop location]
        , fulfillmentId    = Nothing
        , tags             = Nothing
        , items            = Nothing
        , updateTarget     = "order.fullfillments"
        }

  , onBPPReceive = \booking mRide payloadDetails -> do
      -- Unique to AddStop: create location on BPP side, notify driver
      stops <- parseFulfillmentStops payloadDetails
      location <- listToMaybe stops & fromMaybeM (InvalidRequest "No stop")
      locationMapping <- buildLocationMapping location.id booking.id
      QL.create location
      QLM.create locationMapping
      QRB.updateStop booking.id (Just location.id.getId)
      ride <- mRide & fromMaybeM (RideNotFound booking.id)
      Notify.notifyStopModification ride.status ride.driverId booking
      pure Done  -- Immediate: no callback needed

  , onBAPCallback = Nothing  -- Immediate sync model
  }
```

### What "EditDestination" Looks Like — ONLY the Unique Parts

```haskell
editDestRecipe :: ModificationRecipe m
editDestRecipe = ModificationRecipe
  { modName        = "EDIT_LOCATION"
  , validStates    = [TRIP_ASSIGNED, CONFIRMED]
  , validTripTypes = Nothing  -- works for any trip type
  , syncModel      = WithDriverApproval
      { driverPrompt = DriverPromptConfig
          { overlayType = UPDATE_LOC_FCM
          , expirySeconds = 30
          }
      }

  , onBAPInitiate = \booking req -> do
      -- Unique to EditDest: just send the new destination with SOFT_UPDATE status
      pure $ BecknPayloadDetails
        { descriptorCode   = "EDIT_LOCATION"
        , fulfillmentStops = Just [mkStop req.destination]
        , fulfillmentId    = Just req.bppRideId
        , tags             = Nothing
        , items            = Nothing
        , updateTarget     = "order.fullfillments"
        }

  , onBPPReceive = \booking mRide payloadDetails -> do
      -- Unique to EditDest: calculate new route + fare
      ride <- mRide & fromMaybeM (RideNotFound booking.id)
      driverLoc <- getDriverCurrentLocation ride.driverId
      destination <- parseDestination payloadDetails
      newRoute <- calculateRoute driverLoc destination
      newFare <- recalculateFare newRoute booking.fareParams
      -- Return ShowToDriver: framework creates BookingUpdateRequest,
      -- shows FCM to driver, handles accept/reject API
      pure $ ShowToDriver
        (DriverPromptData newRoute newFare destination)
        { onAccept = do
            -- Unique to EditDest: apply the route + fare changes
            applyRouteUpdate newRoute booking ride
            applyFareUpdate newFare booking ride
            createLocationMappings destination booking ride
            Notify.notifyEditDestination ride
            pure $ mkOnUpdatePayload "CONFIRM_UPDATE" booking newFare
        , onReject = do
            pure $ mkOnUpdateErrorPayload "DRIVER_REJECTED" booking
        }

  , onBAPCallback = Just $ \booking payload -> do
      -- Unique to EditDest: update booking with new destination + fare
      case payload.status of
        "CONFIRM_UPDATE" -> do
          updateBookingDestination booking payload.destination
          updateBookingFare booking payload.fare
          QFareBreakup.deleteAllByBookingId booking.id
          QFareBreakup.createMany payload.fareBreakups
          Notify.notifyOnTripUpdate booking
        "SOFT_UPDATE" -> do
          -- Store for rider to review
          createBookingUpdateRequestOnBAP booking payload
        _ -> pure ()
  }
```

### What "AddBaggage" Looks Like — Built After Both Exist

```haskell
-- By the time you build this, the framework handles everything.
-- Developer only writes the 3 unique functions.

addBaggageRecipe :: ModificationRecipe m
addBaggageRecipe = ModificationRecipe
  { modName        = "ADD_BAGGAGE"
  , validStates    = [NEW, CONFIRMED]
  , validTripTypes = Just [isRideOtpTrip]
  , syncModel      = FireAndForget

  , onBAPInitiate = \booking req -> do
      -- Unique: just pass luggage count in tags
      pure $ BecknPayloadDetails
        { descriptorCode   = "ADD_BAGGAGE"
        , fulfillmentStops = Nothing
        , fulfillmentId    = Nothing
        , tags             = Just [mkTag SEARCH_REQUEST_INFO NUMBER_OF_LUGGAGE req.numberOfLuggages]
        , items            = Nothing
        , updateTarget     = "order.fullfillments"
        }

  , onBPPReceive = \booking _mRide payloadDetails -> do
      -- Unique: recalculate fare with luggage
      numberOfLuggages <- parseNumberOfLuggages payloadDetails
      transporterConfig <- getTransporterConfig booking.merchantOperatingCityId
      validateLuggageCount numberOfLuggages transporterConfig.maxNumberOfLuggages
      newFareParams <- recalculateFareWithLuggage booking.fareParams numberOfLuggages
      QFP.create newFareParams
      QRB.updateNumberOfLuggagesAndFare booking.id numberOfLuggages newFareParams
      -- FireAndForget: send callback with updated fare
      pure $ SendCallback (mkOnUpdatePayload booking newFareParams)

  , onBAPCallback = Just $ \booking payload -> do
      -- Unique: update fare on BAP side
      QEBooking.updateEstimatedFare booking.id payload.newFare
      QFareBreakup.deleteAllByBookingId booking.id
      QFareBreakup.createMany payload.fareBreakups
  }
```

### What the Developer Does NOT Write (Framework Handles)

For every modification, regardless of type, the developer **never writes**:
- Booking fetch + existence check
- Merchant fetch
- BPP booking ID extraction
- State validation logic
- Trip type validation logic
- `UpdateBuildReq` construction
- `ACL.buildUpdateReq` call
- `CallBPP.updateV2` call
- Beckn UPDATE envelope (context, orderId, etc.)
- BPP-side Beckn parsing + event type dispatch
- BPP-side booking fetch + validation
- BookingUpdateRequest creation (for WithDriverApproval)
- Redis lock acquisition/release (for WithDriverApproval)
- Driver FCM overlay construction + sending
- Driver accept/reject API endpoint + handler
- Driver response validation (not expired, correct status)
- ON_UPDATE Beckn envelope construction
- BAP-side callback parsing + routing
- Error wrapping + HTTP response formatting

**That's ~200-300 lines of code that was being copy-pasted for every modification. Written once in the framework.**

---

## Layer 3: Main Flow Phases (State Machine)

The same principle applies to the main ride flow, though the shared implementation per phase is less dramatic than for modifications (each main phase is more unique). The value here is primarily in:

1. **Making the state machine explicit and inspectable**
2. **State validation at transitions**
3. **Beckn envelope construction/parsing**

```haskell
-- The flow is a state machine: trigger → handler → new state
-- Each handler is independent, triggered by different events, on different servers

data FlowState
  = Idle | Searching | Quoted | Booked | Assigned
  | InProgress | Completed | Settled | Evaluated | Cancelled

data FlowTransition = FlowTransition
  { from    :: [FlowState]    -- valid source states
  , trigger :: Trigger         -- what event fires this
  , to      :: FlowState       -- resulting state
  , handler :: m ()            -- the phase logic
  }

rideHailingMachine :: [FlowTransition]
rideHailingMachine =
  [ FlowTransition [Idle]                (RiderAPI "search")          Searching   discover
  , FlowTransition [Searching]           (BecknCallback "on_search")  Quoted      storeEstimates
  , FlowTransition [Quoted]              (RiderAPI "select")          Quoted      selectQuote
  , FlowTransition [Quoted]              (RiderAPI "confirm")         Booked      commitBooking
  , FlowTransition [Booked]              (BecknCallback "on_confirm") Assigned    storeAssignment
  , FlowTransition [Assigned]            (DriverAPI "startRide")      InProgress  beginRide
  , FlowTransition [InProgress]          (DriverAPI "endRide")        Completed   endRide
  , FlowTransition [Completed]           (SystemEvent "payment")      Settled     settlePayment
  , FlowTransition [Settled]             (RiderAPI "feedback")        Evaluated   recordFeedback
  -- Cancel from multiple states
  , FlowTransition [Searching..Assigned] (RiderAPI "cancel")          Cancelled   cancelRide
  , FlowTransition [Assigned..InProgress](DriverAPI "cancel")         Cancelled   cancelRide
  ]
```

A new developer reads this and understands the **entire ride lifecycle** in one place. No tracing through 15 files.

---

## Layer 4: Trip Type Policies

Instead of scattered pattern matching on `TripCategory`:

```haskell
-- Each trip type declares its policies in ONE place
data TripPolicy = TripPolicy
  { searchType         :: SearchReqType     -- OneWaySearch, RentalSearch, etc.
  , pricingStrategy    :: PricingStrategy   -- DistanceTime, Package, FlatFare
  , matchingStrategy   :: MatchingStrategy  -- NearestDriver, Auction, Scheduled
  , fulfillmentMode    :: FulfillmentMode   -- RealTimeTracked, QRTicket
  , allowedMods        :: [Text]            -- which modifications are valid
  , stateExtensions    :: [FlowTransition]  -- extra states for this trip type
  }

oneWayPolicy :: TripPolicy
oneWayPolicy = TripPolicy
  { searchType       = OneWaySearch
  , pricingStrategy  = DistanceTimePricing
  , matchingStrategy = NearestDriver
  , fulfillmentMode  = RealTimeTracked
  , allowedMods      = ["EDIT_LOCATION", "CANCEL"]
  , stateExtensions  = []
  }

rentalPolicy :: TripPolicy
rentalPolicy = TripPolicy
  { searchType       = RentalSearch
  , pricingStrategy  = PackagePricing     -- hours + km
  , matchingStrategy = NearestDriver
  , fulfillmentMode  = RealTimeTracked
  , allowedMods      = ["ADD_STOP", "EDIT_STOP", "EDIT_LOCATION", "CANCEL"]
  , stateExtensions  = []
  }

-- Adding a new trip type: ONE definition, not 20 scattered pattern matches
shuttlePolicy :: TripPolicy
shuttlePolicy = TripPolicy
  { searchType       = FixedRouteSearch
  , pricingStrategy  = FlatFarePricing    -- fixed per route
  , matchingStrategy = ScheduledAllocation
  , fulfillmentMode  = ScheduleBased
  , allowedMods      = ["CANCEL"]
  , stateExtensions  =
      [ FlowTransition [Booked] (SystemEvent "scheduleTime") Assigned allocateVehicle
      ]
  }
```

---

## Current Codebase Patterns (Detailed Reference)

### Ride-Hailing Flow (Rider Side)

| Step | Module | Key Function |
|---|---|---|
| Search | `Domain/Action/UI/Search.hs` | `search` |
| Receive estimates | `Domain/Action/Beckn/OnSearch.hs` | `onSearch` |
| Select quote | `Domain/Action/UI/Select.hs` | `select` |
| Confirm booking | `Domain/Action/UI/Confirm.hs` | `confirm` → `SConfirm.confirm` |
| Ride assigned | `Domain/Action/Beckn/OnConfirm.hs` | `onConfirm` |
| Track ride | `Domain/Action/Beckn/OnStatus.hs` | `onStatus` |
| Cancel | `Domain/Action/UI/Cancel.hs` | `cancel` |
| Rate/feedback | `Domain/Action/UI/Feedback.hs` | `feedback` |

### Ride-Hailing Flow (Driver/Provider Side)

| Step | Module | Key Function |
|---|---|---|
| Receive search | `Domain/Action/Beckn/Search.hs` | `search` |
| Show to drivers | `Domain/Action/UI/SearchRequestForDriver.hs` | listing |
| Driver accepts | `Domain/Action/UI/Driver.hs` | offer acceptance |
| Start ride | `Domain/Action/UI/Ride/StartRide.hs` | `startRide` |
| End ride | `Domain/Action/UI/Ride/EndRide.hs` | `endRide` |
| Cancel ride | `Domain/Action/UI/Ride/CancelRide.hs` | `cancelRide` |

### FRFS (Public Transport) Flow

| Step | Module | Key Function |
|---|---|---|
| Search | `Domain/Action/UI/FRFSTicketService.hs` | search flow |
| Quote/OnSearch | `Beckn/ACL/FRFS/OnSearch.hs` | parse catalog |
| Select | `Beckn/ACL/FRFS/Select.hs` | choose categories |
| Init | `Beckn/ACL/FRFS/Init.hs` | payment setup |
| Confirm | `Beckn/ACL/FRFS/Confirm.hs` | finalize booking |
| Ticket issued | `Beckn/ACL/FRFS/OnConfirm.hs` | extract ticket/QR |
| Cancel | `Beckn/ACL/FRFS/Cancel.hs` | refund flow |

### Key Type Definitions

- **TripCategory**: `lib/beckn-spec/src/Domain/Types/Trip.hs` — OneWay, Rental, RideShare, InterCity, CrossCity, Ambulance, Delivery
- **SearchReq ADT**: `SharedLogic/Search.hs` — OneWaySearch, RentalSearch, InterCitySearch, AmbulanceSearch, DeliverySearch, PTSearch, FixedRouteSearch
- **BookingDetails ADT**: `Domain/Types/Booking.hs` — OneWayDetails, RentalDetails, InterCityDetails, etc.
- **QuoteDetails ADT**: `Domain/Types/Quote.hs` — OneWayDetails, RentalDetails, DriverOfferDetails, etc.
- **Booking status**: NEW → CONFIRMED → COMPLETED / CANCELLED
- **Ride status**: NEW → INPROGRESS → COMPLETED / CANCELLED
- **FRFS booking status**: NEW → APPROVED → PAYMENT_PENDING → CONFIRMING → CONFIRMED → CANCELLED

---

## Real-World Domain Research

### Universal Ride-Hailing Pipeline

Every platform (Uber, Lyft, Grab, Gojek, Ola) implements a variant of:

```
Search → Estimate → Select → Confirm → Match/Dispatch →
Navigate → Arrive → Start → Track → Complete → Pay → Rate
```

### How Major Platforms Decompose

- **Uber**: DISCO (dispatch optimization), Marketplace (pricing/surge), Fulfillment (trip execution). Adopted domain-oriented microservice architecture (DOMA) ~2020.
- **Grab**: "Superapp" model — rides, food, delivery share common transport layer. Centralized allocation service across all transport types.
- **Gojek**: 20+ products sharing common primitives (dispatch, payment, notification). Platform-as-a-service where each vertical is a thin layer over shared capabilities.

### Feature Categories (Comprehensive)

**Discovery & Matching**: Address autocomplete, saved places, ride options, fare comparison, nearby drivers map, schedule ride, ride preferences, multi-modal options, smart defaults.

**Pricing & Fare**: Distance-based, time-based, base fare, surge/dynamic, tolls, parking, night charges, waiting charges, cancellation fee, fare cap/minimum, discounts, congestion pricing, return fare (intercity), platform fee, tax.

**In-Ride**: Real-time tracking, ETA updates, route display, share trip, SOS/emergency, messaging, calling, add stop, change destination, navigation, OTP verification, wait timer, toll detection, safety recording, speed alerts.

**Post-Ride**: Rating (1-5), review tags, tip, receipt, lost item, dispute, fare adjustment, refund, re-booking.

**Driver Management**: Onboarding, document verification, background check, vehicle inspection, go online/offline, earnings dashboard, acceptance/cancellation rate tracking, performance score, incentives, penalties, heat maps, payouts, leaderboard, training, subscription plans, shift management.

**Customer Features**: Ride history, favorite drivers, loyalty program, subscription, family account, accessibility, language preference, referral, corporate profile, scheduled rides.

**Operations**: Surge pricing engine, geofencing, supply management, demand forecasting, fraud detection, customer support, incident management, A/B testing, city launch toolkit, dashboard, analytics.

**Compliance**: Regulatory licensing, driver permits, insurance, data privacy, tax reporting, accessibility compliance, price regulation, safety regulations, audit trails, government reporting.

### Future Possibilities

- **Multi-modal transport**: Journey entity spanning multiple legs (auto → metro → walk) with unified booking and fare
- **Autonomous vehicles**: Driver becomes optional; Vehicle gains autonomous capability flags; remote monitoring
- **Advanced pricing**: Personalized, route-based, event-aware, carbon pricing, yield management
- **Real-time behavior scoring**: Telematics-based safety score, dynamic matching by trust level, behavioral nudges
- **Predictive dispatch**: ML-based demand prediction, driver pre-positioning, anticipatory routing
- **Carbon-aware routing**: Emission estimation per ride, green options, eco-routing

### Domain Events (For Future Event-Driven Architecture)

| Event | Emitter | Consumers |
|---|---|---|
| `RideRequested` | Booking | Matching, Analytics, Notification |
| `DriverAccepted` | Matching | Rider notification, Ride execution |
| `RideConfirmed` | Booking | Payment (hold), Notification, Safety |
| `DriverArrived` | Ride execution | Rider notification, Wait timer |
| `RideStarted` | Ride execution | Tracking, Fare meter, Safety monitoring |
| `LocationUpdated` | Driver/Rider app | Tracking, ETA, Route deviation |
| `RideCompleted` | Ride execution | Fare calculation, Payment, Rating prompt |
| `PaymentCompleted` | Payment | Receipt, Driver earnings, Settlement |
| `RideCancelled` | Booking | Cancellation fee, Re-matching, Analytics |
| `SOSTriggered` | Safety | Emergency response, Admin alert |

---

## Service Architecture: Current State

### Current Project Structure

| Service | Cabal Project | Hand-written Modules | Generated Modules | Role |
|---------|--------------|---------------------|-------------------|------|
| rider-app | `rider-app` | 750 | 755 | BAP — rider-facing APIs + Beckn callbacks |
| driver-app | `dynamic-offer-driver-app` | 867 | 834 | BPP — driver-facing APIs + Beckn handlers |
| allocator | `driver-offer-allocator` | 3 | 0 | Thin wrapper over driver-app for offer allocation |
| rider-scheduler | `rider-app-scheduler` | 3 | 0 | Thin wrapper over rider-app for scheduled jobs |
| dashboards | 4 projects | ~100 | ~50 | Admin APIs consuming rider-app + driver-app |

**Key structural facts:**
- rider-app and driver-app are **completely independent** — neither imports the other
- They communicate **only via Beckn HTTP** calls
- Allocator and scheduler are tiny wrappers that depend on the main apps
- 21 shared libraries in `lib/` (beckn-spec, utils, payment, etc.)
- A single flow like "add stop" has its logic **physically split across two independent codebases** that can't reference each other

### The Problem This Creates

A "ride flow" is one logical thing, but its code is scattered:

```
                    ┌─────────────────────────────┐
  rider-app         │  Domain/Action/UI/Booking.hs │  ← BAP-side add stop logic
  (750 modules)     │  Beckn/ACL/Update.hs         │  ← BAP builds Beckn UPDATE
                    │  Domain/Action/Beckn/OnUpdate │  ← BAP handles callback
                    └──────────────┬──────────────┘
                                   │ Beckn HTTP (the ONLY connection)
                    ┌──────────────┴──────────────┐
  driver-app        │  Beckn/ACL/Update.hs         │  ← BPP parses Beckn UPDATE
  (867 modules)     │  Domain/Action/Beckn/Update   │  ← BPP processes modification
                    │  Domain/Action/UI/EditBooking │  ← Driver accept/reject
                    └─────────────────────────────┘
```

- **Developer working on "add stop" must touch 2 independent projects**
- **No way to see the complete flow in one place**
- **No way to test the full flow without running both services**
- **Common logic (booking fetch, state validation) is duplicated in both projects**

---

## Migration Strategy

### Core Architectural Change: Transport-Agnostic Flow Library

The key idea: **a flow recipe defines BOTH BAP and BPP sides in one place, in a shared library.** The transport between them (Beckn HTTP vs direct function call) is abstracted.

```
                    ┌─────────────────────────────────────────┐
  lib/              │  MobilityFlow/                          │
  mobility-flows/   │    Modifications/AddStop.hs      ← BOTH sides defined here
                    │    Modifications/EditDest.hs     ← BOTH sides defined here
                    │    Flows/RideHailing.hs          ← Full state machine
                    │    Flows/PublicTransit.hs         ← Full state machine
                    │    Core/Framework.hs             ← Shared implementation
                    │    Core/Transport.hs             ← Async/Sync abstraction
                    └────────────┬────────────────────────────┘
                                 │  (both services depend on this)
                    ┌────────────┴────────────┐
                    │                         │
            ┌───────┴──────┐          ┌───────┴──────┐
            │  rider-app   │          │  driver-app  │
            │  (thin shell)│          │  (thin shell)│
            │  Wires BAP   │          │  Wires BPP   │
            │  side only   │          │  side only   │
            └──────────────┘          └──────────────┘
```

### Transport Abstraction: Sync vs Async

```haskell
-- The transport layer is pluggable
class FlowTransport t m where
  -- BAP → BPP communication
  sendUpdate    :: t -> Booking -> BecknPayloadDetails -> m ()
  -- BPP → BAP communication
  sendOnUpdate  :: t -> Booking -> OnUpdatePayload -> m ()
  sendOnError   :: t -> Booking -> ErrorPayload -> m ()

-- PRODUCTION: Beckn HTTP (async, two services)
data BecknHTTP = BecknHTTP
instance FlowTransport BecknHTTP m where
  sendUpdate t booking payload = do
    becknMsg <- ACL.buildUpdateReq (mkUpdateBuildReq booking payload)
    CallBPP.updateV2 booking.providerUrl becknMsg
  sendOnUpdate t booking payload = do
    becknMsg <- ACL.buildOnUpdateReq booking payload
    CallBAP.sendOnUpdate booking.bapUri becknMsg

-- SYNC MODE: Direct function call (one process, for testing or simple deploys)
data DirectCall = DirectCall
  { bppHandler :: Booking -> BecknPayloadDetails -> m BPPOutcome
  , bapHandler :: Booking -> OnUpdatePayload -> m ()
  }
instance FlowTransport DirectCall m where
  sendUpdate t booking payload = do
    outcome <- t.bppHandler booking payload
    case outcome of
      Done -> pure ()
      SendCallback onUpdate -> t.bapHandler booking onUpdate
      ShowToDriver _ -> error "DirectCall doesn't support driver approval"
  sendOnUpdate t booking payload = t.bapHandler booking payload
```

This means the same recipe can run:

```haskell
-- Async (production): BAP sends HTTP, BPP processes separately
runModification @BecknHTTP addStopRecipe req

-- Sync (testing): entire flow runs in one process, one function call
runModification @DirectCall addStopRecipe req

-- Sync (integration test): full flow, both sides, in-memory
runFullFlowSync rideHailingMachine searchReq
  -- Runs: search → on_search → select → confirm → on_confirm → ... → endRide
  -- All in one process, no HTTP, deterministic
```

### How Sync Mode Enables Full-Flow Testing

```haskell
-- Today: to test a ride flow end-to-end, you need:
--   1. Start rider-app server
--   2. Start driver-app server
--   3. Start Redis, Postgres, Kafka
--   4. Make HTTP calls in sequence, wait for callbacks
--   5. Manually verify state at each step

-- With sync mode:
testRentalRideWithStop :: TestM ()
testRentalRideWithStop = do
  -- Run entire flow synchronously in one process
  result <- runFlowSync rideHailingMachine $ do
    searchResult  <- triggerPhase "search" rentalSearchReq
    estimates     <- triggerPhase "on_search" searchResult
    quote         <- triggerPhase "select" (selectEstimate estimates)
    booking       <- triggerPhase "confirm" (confirmQuote quote)
    ride          <- triggerPhase "on_confirm" booking
    startedRide   <- triggerPhase "startRide" (startReq ride)
    -- Now test the modification mid-flow
    modResult     <- triggerMod addStopRecipe (AddStopReq ride.id someLocation)
    completedRide <- triggerPhase "endRide" (endReq startedRide)
    pure completedRide

  -- Assert on final state
  result.ride.status `shouldBe` COMPLETED
  result.booking.stops `shouldContain` [someLocation]
```

### New Library Structure

```
lib/mobility-flows/
  package.yaml
  src/
    MobilityFlow/
      Core/
        Framework.hs           -- runModification, runPhase (shared implementation)
        Transport.hs           -- FlowTransport typeclass + instances
        StateMachine.hs        -- FlowState, FlowTransition, state validation
        Types.hs               -- ModificationRecipe, BecknPayloadDetails, SyncModel, etc.
        BecknEnvelope.hs       -- Common Beckn UPDATE/ON_UPDATE construction
        DriverPrompt.hs        -- BookingUpdateRequest + FCM overlay (shared impl)

      Modifications/
        AddStop.hs             -- recipe (~40 lines of unique logic)
        EditDestination.hs     -- recipe (~60 lines of unique logic)
        ChangeServiceTier.hs   -- recipe (~30 lines of unique logic)
        AddBaggage.hs          -- recipe (~30 lines of unique logic)
        Registry.hs            -- Map of all registered modifications

      Flows/
        RideHailing.hs         -- state machine definition + phase handlers
        PublicTransit.hs       -- state machine definition + phase handlers

      Policies/
        Pricing.hs             -- pricing strategies (DistanceTime, Package, FlatFare)
        Matching.hs            -- matching strategies (Nearest, Auction, Scheduled)
        Fulfillment.hs         -- fulfillment modes (RealTimeTracked, QRTicket)

      TripTypes/
        OneWay.hs              -- TripPolicy for one-way
        Rental.hs              -- TripPolicy for rental
        InterCity.hs           -- TripPolicy for intercity
        Registry.hs            -- Map of all trip type policies
```

Each service becomes a thin shell:

```
rider-app/src/
  Wiring/
    BAP.hs                     -- registers BAP-side handlers from mobility-flows
    ModificationHandlers.hs    -- routes Beckn callbacks to modification framework
    FlowPhaseHandlers.hs       -- routes API calls to flow phase handlers
  -- ... remaining rider-specific modules (person, profile, saved places, etc.)

driver-app/src/
  Wiring/
    BPP.hs                     -- registers BPP-side handlers from mobility-flows
    ModificationHandlers.hs    -- routes Beckn UPDATE to modification framework
    DriverUIHandlers.hs        -- routes driver API calls to flow handlers
  -- ... remaining driver-specific modules (onboarding, fleet, etc.)
```

### Module Reduction Impact

| Area | Current (rider + driver) | After Migration | Reduction |
|------|--------------------------|-----------------|-----------|
| Modification Domain Actions | ~12 files × 4 mods = ~48 | 4 recipe files + 1 framework | ~43 files |
| Beckn ACL Update builders | ~4 per service = ~8 | 1 BecknEnvelope.hs | ~7 files |
| Beckn ACL Update parsers | ~4 per service = ~8 | Handled by framework | ~8 files |
| Main flow Domain Actions | ~30 per service = ~60 | ~15 flow phases (shared) + 2 wiring files | ~43 files |
| Main flow Beckn ACL | ~14 per service = ~28 | Handled by framework | ~20 files |
| SharedLogic (duplicated) | ~20 duplicated across services | Moved to mobility-flows | ~20 files |
| **Total** | **~172 files** | **~30 files** | **~140 files** |

Note: this only covers flow-related modules. The remaining ~1400 modules per service are domain-specific (driver onboarding, fleet management, rider profile, etc.) and aren't affected.

---

## Migration Path: How Existing Code Gets Deprecated

The migration is 5 levels. Each level is independently shippable. At every level, the OLD code is used by the new framework before it's replaced — nothing is deleted until the new version is proven identical.

### Level 1: Wrap (Framework wraps existing code)

New framework calls existing handler functions via thin adapters. Old code unchanged.

```
SearchPhase.spBapSearch = adapter that calls existing Search.search with BAPContext
SearchPhase.spOutboundLink = PhaseLink using existing ACL.buildSearchReqV2 / ACL.parseSearchReq
SearchPhase.spBppSearch = adapter that calls existing BPP.Search.handler
```

**Deprecated**: Nothing
**Value**: Sync testing, flow visible in one place, state validation

### Level 2: Route (API routes go through framework)

API routes call the framework (which validates state) instead of calling handlers directly.

```
OLD: API/UI/Search.hs → Search.search
NEW: Wiring/BAP.hs → validateTransition → searchPhase.spBapSearch → Search.search
```

**Deprecated**: API route files in rider-app/driver-app (routes move to thin shell wiring)
**Value**: State validation enforced by framework at every entry point

### Level 3: Absorb Handlers (Handler logic moves into phases)

The adapter wrappers get inlined. Handlers take BAPContext/BPPContext directly instead of 11 scattered params.

**Deprecated**: `Domain/Action/UI/Search.hs`, `Domain/Action/Beckn/OnSearch.hs`, etc. — logic lives in phase handlers
**Value**: Clean handler signatures, no more 11-param functions

### Level 4: Absorb ACL (ACL becomes PhaseLinks)

Existing ACL code from both rider-app and driver-app gets reorganized into PhaseLink pairs in mobility-flows.

```
OLD: rider-app/Beckn/ACL/Search.hs + driver-app/Beckn/ACL/Search.hs (4 separate files)
NEW: mobility-flows/ACL/Search.hs with searchOutboundLink + searchCallbackLink (1 file, paired)
```

**Deprecated**: All `Beckn/ACL/*.hs` files in both projects
**Value**: ACL for both directions lives together, obvious what pairs with what

### Level 5: Thin Shell (Framework IS the app)

rider-app and driver-app become thin shells: API routes → framework → handlers. All flow logic in mobility-flows.

```
OLD rider-app: 750 hand-written modules (flow + non-flow mixed)
NEW rider-app-v2: ~50 modules (wiring + rider-specific features only)
```

**Deprecated**: All flow-related modules in old rider-app/driver-app
**Value**: One codebase for the flow, thin deployment shells

### File-by-File Deprecation (Search Phase Example)

| Existing File | L1 (Wrap) | L2 (Route) | L3 (Handler) | L4 (ACL) | L5 (Shell) |
|---|---|---|---|---|---|
| `rider-app/API/UI/Search.hs` | Used | Deprecated | - | - | Thin shell route |
| `rider-app/Domain/Action/UI/Search.hs` | Called by adapter | Called by adapter | Logic moved to phase | - | Gone |
| `rider-app/Beckn/ACL/Search.hs` | Called by PhaseLink | Called | Called | Moved to PhaseLink | Gone |
| `driver-app/Beckn/ACL/Search.hs` | Called by PhaseLink | Called | Called | Moved to PhaseLink | Gone |
| `driver-app/Domain/Action/Beckn/Search.hs` | Called by adapter | Called | Logic moved to phase | - | Gone |
| `rider-app/SharedLogic/CallBPP.hs` | Called | Called | Called | Called | Replaced by transport |

### Non-Flow Code (Unchanged)

~400 modules per service that aren't flow logic (driver onboarding, fleet management, rider profile, saved places, SOS, insurance, leaderboard, etc.) stay in their respective services. The thin shell handles these directly.

---

## Migration Phases (Incremental)

### Phase 0 — Create `lib/mobility-flows` Skeleton (DONE)
- Library created with Core/Types, Core/Transport, Core/StateMachine, Core/PhaseLink, Core/Context
- Flows/RideHailing with state machine + per-phase records (SearchPhase, SelectPhase, etc.)
- Core/FlowRunner with runSearchPhaseSync + validateAndTransition
- Registered in cabal.project, builds with GHC 9.2.7

### Phase 1 — Wire Search Phase End-to-End (Level 1: Wrap)
- Add FlowDecisions for rider/driver/third-party decision points
- Add mobility-flows as dependency in hunit-tests
- Wire SearchPhase with real types (adapters around existing handlers + ACL)
- Run runSearchPhaseSync end-to-end
- Validate: estimates match what async mode produces

### Phase 2 — Wire Remaining Phases
- SelectPhase, InitPhase, ConfirmPhase, RideExecPhase, CancelPhase
- runFlowSync chains all phases with FlowDecisions
- Full ride-hailing flow testable in sync mode

### Phase 3 — Modification Framework
- runModification shared implementation
- AddStop, EditDestination, ChangeServiceTier, AddBaggage as recipes
- Side-by-side validation against existing code

### Phase 4 — API Routes Through Framework (Level 2: Route)
- rider-app-v2 wiring routes API calls through framework
- driver-app-v2 wiring routes Beckn events through framework
- State validation enforced at every entry point

### Phase 5 — Trip Type Policies
- TripPolicy records per trip type
- Migrate scattered TripCategory pattern matching

### Phase 6 — Absorb Handlers + ACL (Level 3-4)
- Handler logic moves into phase handlers
- ACL reorganized into PhaseLink pairs
- Old handler/ACL files deprecated

### Phase 7 — Thin Shell Transition (Level 5)
- rider-app-v2 fully functional
- driver-app-v2 fully functional
- Old flow modules deprecated

---

## What Changes vs What Stays

### Changes

| What | From | To |
|------|------|----|
| Flow definition | Implicit, scattered across 15+ files per flow | Explicit state machine + phase records in one file |
| Handler signatures | 11-param functions with scattered context | BAPContext/BPPContext + domain params |
| BAP ↔ BPP coordination | Only via Beckn HTTP, code split across projects | Defined together in shared library, transport pluggable |
| Beckn ACL | Separate files per direction per project | PhaseLink pairs (toWire + fromWire together) |
| Modification logic | ~500 lines across ~12 files, repeated boilerplate | ~40 line recipe + shared framework |
| Trip type branching | Pattern matching on TripCategory in 20+ places | One TripPolicy record per type |
| Testing | Requires running multiple servers | Sync mode with FlowDecisions for user/driver inputs |

### Stays the Same

| What | Why |
|------|-----|
| NammaDSL for storage/API generation | Orthogonal — generates types and queries, building blocks use them |
| Deployment topology (separate services) | Services still deploy separately, just share a library |
| Beckn protocol on the wire | Production still uses Beckn HTTP, sync mode is optional |
| Domain types (Booking, Ride, etc.) | Building blocks operate on existing types, no migration needed |
| Database schemas | No table changes, same atlas_app / atlas_driver_offer_bpp |
| Service-specific features (onboarding, fleet, profile) | Not flow logic — stays in respective services |
| Allocator, scheduler, dashboards | Unaffected, remain thin wrappers |
