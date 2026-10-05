# Booking/Ride State Machine — Unification Plan

Status: PLANNED (scoped 2026-09-28). Follows the Sept 2026 unification audit
(`unification-audit-2026-09.md`, item #2). Track A (tag schema) shipped first; this is next.

## Problem

There is no central state machine for Booking/Ride status. Evidence (verified 2026-09-28):

- `BookingStatus` defined twice at the domain layer: beckn-spec (7 constructors) and
  driver-app src-read-only (5 — missing `CONFIRMED`, `AWAITING_REASSIGNMENT`, i.e. a strict
  subset). `RideStatus` defined twice with identical constructors. Rider-app already uses
  beckn-spec's enums exclusively; **only driver-app needs consolidation**.
- Status writes: 4 booking writer functions + 7 ride writer functions across the two apps,
  ~40 call sites. Exactly ONE transition is race-guarded anywhere (rider
  `BookingExtra.updateStatus`, `CONFIRMED <- IN [NEW]`, with a TODO admitting the rest).
  Driver-app's `updateStatus` has **no guard at all**.
- ~570 ad-hoc `.status ==`/`elem` comparisons; 81 `BookingInvalidStatus`/`RideInvalidStatus`
  throws; 4 conflicting local `isValidRideStatus` definitions; driver-app has **no named
  status sets** (15+ inline lists like `[Ride.INPROGRESS, Ride.NEW]`), rider-app has
  `activeBookingStatus`/`terminalBookingStatus`/`activeScheduledBookingStatus` in
  `Domain/Types/Extra/Booking.hs:29-36` (which partition the 7 constructors exactly 4+3).
- Rider `updateStatus` mutates the ACBL active-booking Redis cache **before** the guarded
  DB write — a lost CAS already leaves a stale cache entry today.
- Template precedent: `lib/finance-kernel/src/Lib/Finance/StateMachine/` — right shape
  (transition map, `isValidTransition`) but zero external callers, `Either`-based (clashes
  with `throwError` convention), event-driven, persists every transition, and its table
  covers only 4/14 states (silent deny-list). We borrow the table idea, not the machinery.

## Design decisions

1. **State-based table (`from -> [to]`), not event-based.** All existing code reasons in
   states; finance-kernel's `from × event -> to` shape would force an event vocabulary
   nobody has. Terminality = no outgoing edges (like finance-kernel), with named sets
   *derived* from the table.
2. **Table + pure validators live in beckn-spec** next to the enums (both apps already
   depend on beckn-spec; no shared-kernel change, no pin bump).
3. **Per-app CAS wrappers** (`transitionBooking` / `transitionRide`), because the writer
   signatures are genuinely asymmetric (rider needs `Id Person` for the ACBL cache; driver
   pairs status writes with LTS/driver-mode side effects). The wrappers share the beckn-spec
   table; the SQL predicate is `Se.Is Beam.status (Se.In allowedFromStates)` — mechanism
   already proven in production by the rider CONFIRMED case (works through the KV/drainer
   layer).
4. **Shadow mode before enforcement.** `updateOneWithKV` returns `m ()` (no affected-row
   count), and the table is reverse-engineered from call sites — some legitimate transition
   could be missing. So Phase 2 lands in two steps: (2a) validate against the table, log
   `logError "INVALID_STATUS_TRANSITION"` on violation, write unguarded (zero behavior
   change); (2b) after a soak period with clean logs, add the WHERE-clause CAS. Callers that
   must distinguish "applied" from "lost the race" re-read after the write (opt-in
   `transitionBookingChecked` returning the post-state); default is fire-and-forget.
   (Plumbing a row count through `Kernel.Beam.Functions` would be better long-term but is a
   shared-kernel change — deliberately avoided for now.)
5. **Two guard kinds, treated differently.** Validation guards
   (`unless ... $ throwError BookingInvalidStatus`) keep throwing — they're API semantics,
   often far from the write. Idempotence guards immediately before writes
   (`unless (booking.status == target) $ update...`) become CAS no-ops and get deleted in
   Phase 3.

## Proposed transition tables (union of every write observed in the code)

```haskell
-- lib/beckn-spec/src/Domain/Types/BookingStatus.hs
validBookingTransitions :: Map BookingStatus [BookingStatus]
  NEW                   -> [CONFIRMED, TRIP_ASSIGNED, CANCELLED]        -- TRIP_ASSIGNED: one-shot assign path (Beckn/Common.hs:740 pre-persisted)
  CONFIRMED             -> [TRIP_ASSIGNED, AWAITING_REASSIGNMENT, CANCELLED, REALLOCATED]
  TRIP_ASSIGNED         -> [COMPLETED, CANCELLED, AWAITING_REASSIGNMENT, REALLOCATED]
  AWAITING_REASSIGNMENT -> [TRIP_ASSIGNED, REALLOCATED, CANCELLED]
  COMPLETED             -> []   -- terminal
  CANCELLED             -> []   -- terminal
  REALLOCATED           -> []   -- terminal

-- lib/beckn-spec/src/Domain/Types/RideStatus.hs
validRideTransitions :: Map RideStatus [RideStatus]
  UPCOMING   -> [NEW, CANCELLED]          -- scheduled-ride activation (ScheduledRideAssignedOnUpdate.hs:187) — the one "backwards" edge, must be whitelisted
  NEW        -> [INPROGRESS, CANCELLED]
  INPROGRESS -> [COMPLETED, CANCELLED]
  COMPLETED  -> []
  CANCELLED  -> []
```

Derived (replacing today's hand-maintained/inline lists):
`activeBookingStatus = [s | s has outgoing edges]`, `terminalBookingStatus = [s | no edges]`,
`activeRideStatus = [UPCOMING, NEW, INPROGRESS]`, plus `activeScheduledBookingStatus` kept
explicit. **Open question for review:** the dashboard booking-sync paths
(`Dashboard/Booking.hs:166`, driver `Dashboard/Management/Booking.hs:149`) derive booking
status from ride status and may perform ops-unstick jumps; confirm the table admits them or
give the dashboard path an explicit `forceTransitionBooking` (logged, dashboard-only).
Shadow mode (2a) exists precisely to catch edges this table misses.

## Phases

### Phase 1 — one enum + named sets (self-contained PR, no behavior change)
1. Driver-app `spec/Storage/Booking.yaml`: delete the inline `types: BookingStatus:` block
   (lines ~31-35), add `BookingStatus: Domain.Types.BookingStatus` to `imports:`. Same for
   `Ride.yaml` (`RideStatus: Domain.Types.RideStatus`). Precedent: rider Booking.yaml:18 +
   ride.yaml:14 already do exactly this; driver ScheduledBooking API spec already imports
   beckn-spec's RideStatus. Run `, run-generator`.
2. Add beam orphan instances in a driver-app module mirroring rider's
   `Domain/Types/Common.hs`: `$(mkBeamInstancesForEnum ''BookingStatus)`,
   `$(mkBeamInstancesForEnumAndList ''RideStatus)` (+ ClickhouseValue if needed).
   Show/Read text mapping over varchar → **no data migration**. Driver DB never contains
   `CONFIRMED`/`AWAITING_REASSIGNMENT`, and a superset enum reads old rows fine.
3. Expected fallout to fix (the real Phase-1 work): every exhaustive `case` on driver-app
   `BookingStatus` becomes non-exhaustive under `-Werror` (2 new constructors). Each new
   branch is unreachable on the BPP today — handle with explicit, commented catch-alls,
   not wildcards, so the compiler keeps guarding future divergence. Delete the
   `castRideStatus`/booking casts at driver `Dashboard/Management/ScheduledBooking.hs:680-697`.
4. Move `activeBookingStatus`/`terminalBookingStatus`/`activeScheduledBookingStatus` from
   rider `Domain/Types/Extra/Booking.hs` to beckn-spec (re-export from the old site to keep
   the 27 call sites compiling); add `activeRideStatus`; replace driver-app's ~20 inline
   status lists (`BookingExtra.hs:230`, `RideExtra.hs:131,251,...`, `CancelRide.hs:300`,
   `StartRide.hs:364`, ...) and rider's ad-hoc `activeBookingStatus <> [COMPLETED]`
   (JourneyLeg/Taxi.hs) with named sets.

### Phase 2a — transition table + shadow validation (no behavior change)
1. Add tables + `isValidBookingTransition` / `isValidRideTransition` + derived sets to
   beckn-spec, with a `Bounded`/`Enum` exhaustiveness spec: every constructor is either a
   key or explicitly terminal (the finance-kernel silent-deny-list lesson).
2. Wrap the 11 writer functions (4 booking: rider/driver `updateStatus` + 2 bulk
   `cancelBookings`; 7 ride: rider `updateStatus`/`updateMultiple`/`cancelRides`, driver
   `updateStatus`/`updateStatusAndRideEndedBy`/`updateStatusByIds`/`updateAll`) with a
   shared pre-write validate-and-log: read current status (all single-row callers already
   have the entity in hand — pass it in, no extra read), check table, `logError` with a
   grep-able tag on violation, then write as today.
3. Fix the ACBL cache ordering in rider `updateStatus`: mutate cache **after** the DB write.
4. Soak in prod; grep logs; amend the table where legitimate edges surface.

### Phase 2b — enforce (CAS)
1. Single-row writers: add `Se.Is Beam.status (Se.In (allowedFrom target))` to the WHERE.
   Bulk writers: add `AND status IN (...)` to the bulk WHERE (stuck-booking cancellers
   already pre-filter on the same sets, so this is belt-and-braces).
2. Idempotent by construction: same-status re-writes (the OnStatus/Common.hs idempotence
   guards) become zero-row no-ops.
3. Opt-in `transitionBookingChecked`/`transitionRideChecked` (post-write re-read) for the
   few callers that branch on success — e.g. OnConfirm's CONFIRMED write.

### Phase 3 — caller cleanup
1. `assertBookingStatusIn` / `assertRideStatusIn` helpers next to the enums; migrate the 81
   throw sites (top files: rider Beckn/Common.hs 19, driver Beckn/Update.hs 15, rider
   OnUpdate.hs 9, rider UI/Booking.hs 10). Adopt OnCancel.hs:140's allowed-from-set
   predicate style as the template.
2. Delete idempotence guards made redundant by CAS.
3. Fix en passant: rider `OnUpdate.hs:746` throws the literal string
   `"$ show booking.status"` (interpolation bug from the audit).

## Explicitly out of scope (documented, not touched)
- public-transport-rider-platform's legacy Esqueleto `updateStatus` (separate app/table).
- Dashboard API-layer status enums (different vocabulary, API contracts) and
  IssueManagement's filter enums.
- The denormalized status copies (LTS `rideStart/rideEnd`, kafka event payloads,
  `DriverInformation.onRide`, rider `PersonFlowStatus`, driver scheduled-booking Redis
  index). The CAS reduces how often they diverge but does not unify them — candidate for a
  later phase where side effects hang off the transition function.

## Risks
- **Missing edges in the table** → shadow mode (2a) is the mitigation; nothing is blocked
  until logs are clean.
- **Driver-app exhaustive-case fallout** in Phase 1 is unbounded until measured — first
  implementation step is `grep case ... booking.status` over driver-app to size it.
- **KV/drainer semantics of `Se.In` predicates on bulk updates** are unexercised (single-row
  CONFIRMED case is the only precedent) — verify with the KV layer once in 2b, or keep bulk
  writers in shadow mode longer.
- Beckn `on_status` sync paths intentionally jump states when BAP/BPP disagree — those go
  through the same table; if they legitimately need wider edges, they get the logged
  `forceTransition` escape hatch rather than widening the table for everyone.
