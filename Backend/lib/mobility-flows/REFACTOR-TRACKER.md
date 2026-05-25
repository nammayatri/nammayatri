# Building Blocks Refactor — Cross-Session Tracker

## Status: Phase 0 — ALMOST COMPLETE. Next: compile, then wire real test in hunit-tests.

## Rules (MUST follow in every session)

### R1: NEVER modify existing code
- All new code goes in NEW folders/files
- Existing rider-app, driver-app code stays untouched
- New thin shells: `app/rider-platform/rider-app-v2/`, `app/provider-platform/driver-app-v2/`
- New shared lib: `lib/mobility-flows/`
- Old code gets deprecated ONLY when new code is fully working and tested

### R2: One modification at a time
- Migrate one modification recipe at a time (AddStop → AddBaggage → ChangeServiceTier → EditDestination)
- Each must pass integration tests before moving to next
- Never have two modifications half-migrated simultaneously

### R3: Side-by-side validation
- New recipes must produce IDENTICAL Beckn messages as old code
- Write comparison tests: run old code path and new code path, diff the outputs
- Only switch traffic to new code after validation

### R4: Track everything
- Update this file at END of every session with: what was done, what's next, any blockers
- Mark phases/items as DONE/IN_PROGRESS/TODO/POSTPONED/BLOCKED

### R5: Minimal dependencies
- `lib/mobility-flows/` should depend on `beckn-spec`, `utils`, and minimal others
- It should NOT depend on rider-app or driver-app (that would create circular deps)
- Shared domain types (Booking, Ride, etc.) must be accessible — may need to extract to a shared types lib

### R6: Compile only when user asks
- The codebase uses `-Werror` — no warnings allowed
- Don't git add or compile without user asking

### R7: No feature flags in production code
- Old and new code coexist as separate modules/folders, not as runtime branches
- Switching happens at the API routing level, not inside business logic

### R8: Document decisions
- When making a non-obvious architectural choice, add a comment in the code AND note it here
- When postponing something, record WHY and WHAT would unblock it

### R9: Use real types, no hacks
- No `Value` types — use proper typed Haskell data structures
- No `Maybe` hacks for fields that don't apply in certain phases — use proper per-phase types or typeclasses
- All types must align with existing rider-app / driver-app codebase for backward compatibility
- Tests use REAL domain types from existing codebase, not fake parallel types

## Progress

### Phase 0: Setup
- [x] Create `lib/mobility-flows/` with package.yaml and module structure
- [x] Create `app/rider-platform/rider-app-v2/` thin shell skeleton
- [x] Create `app/provider-platform/driver-app-v2/` thin shell skeleton
- [x] Define Core/Types.hs (Side, SyncModel, BPPOutcome, DriverPromptData — all strongly typed)
- [x] Define Core/Transport.hs (FlowTransport typeclass — typed msg, no Value)
- [x] Define Core/StateMachine.hs (FlowTransition, Trigger, validateTransition)
- [x] Define Flows/RideHailing.hs:
  - [x] RideFlowState enum mapped to existing BookingStatus/RideStatus
  - [x] rideFlowTransitions — complete lifecycle including init/on_init
  - [x] Per-phase records (SearchPhase, SelectPhase, InitPhase, ConfirmPhase, RideExecPhase, CancelPhase)
  - [x] RideHailingFlow record — composition of all phase records
- [x] Define Core/PhaseLink.hs (PhaseLink: toWire + fromWire bundle, runLinkSync)
- [x] Define Core/Context.hs (BAPContext, BPPContext, ClientInfo — parameterized by entity types)
- [x] Define Core/FlowRunner.hs (runSearchPhaseSync + validateAndTransition)
- [x] Scaffold Flows/PublicTransit.hs with state enums
- [x] Scaffold Modifications/Registry.hs
- [x] Scaffold thin shell Wiring/BAP.hs and Wiring/BPP.hs
- [x] Register mobility-flows in cabal.project
- [x] `cabal build mobility-flows` passes (verified earlier, needs recheck after latest changes)
- [ ] Recheck `cabal build mobility-flows` after PhaseLink/Context/FlowDecisions changes
- [x] Update domain-building-blocks.md with migration path (Level 1-5 deprecation strategy)

### Phase 1: Wire Search Phase End-to-End
- [x] Study existing handler signatures
- [x] Define FlowDecisions (rider/driver/third-party decision points at pause points)
- [x] Update FlowRunner with runFlowSync (chains all phases + FlowDecisions)
- [x] Update FlowRunner with per-phase sync runners (runSearchPhaseSync, runSelectPhaseSync, etc.)
- [ ] Compile mobility-flows with all new code
- [ ] Add mobility-flows as dependency in hunit-tests/package.yaml
- [ ] Create hunit-tests/src/RideFlowTest.hs with SearchPhase wired to REAL types
- [ ] Wire spBapSearch adapter → calls existing Search.search with BAPContext
- [ ] Wire spOutboundLink.toWire → calls existing ACL.buildSearchReqV2
- [ ] Wire spOutboundLink.fromWire → calls existing BPP-side ACL parser
- [ ] Wire spBppSearch → calls existing Domain.Action.Beckn.Search.handler
- [ ] Wire spCallbackLink.toWire → calls existing BPP on_search ACL builder
- [ ] Wire spCallbackLink.fromWire → calls existing BAP on_search ACL parser
- [ ] Wire spBapOnSearch → calls existing Domain.Action.Beckn.OnSearch.onSearch
- [ ] Run runSearchPhaseSync end-to-end in hunit-tests
- [ ] Verify: estimates produced by sync mode match what async mode would produce
- [ ] Run runFlowSync end-to-end with real types in test
- [ ] Validate state machine transitions match actual booking/ride status changes

### Phase 2: Modification Framework
- [ ] Implement Core/Framework.hs (runModification, handleBPPUpdate, handleDriverResponse, handleBAPOnUpdate)
- [ ] Implement Core/BecknEnvelope.hs (common UPDATE/ON_UPDATE construction)
- [ ] Implement Core/DriverPrompt.hs (BookingUpdateRequest + FCM)
- [ ] Write AddStop as first recipe
- [ ] Side-by-side test: old AddStop vs new AddStop produce identical Beckn messages

### Phase 3: Migrate Modifications
- [ ] AddStop recipe (simplest — Immediate sync model)
- [ ] AddBaggage recipe (FireAndForget sync model)
- [ ] ChangeServiceTier recipe (FireAndForget sync model)
- [ ] EditDestination recipe (WithDriverApproval — most complex)
- [ ] Wire rider-app-v2 to route modifications through framework
- [ ] Wire driver-app-v2 to route modifications through framework

### Phase 4: Sync Mode + Testing
- [ ] Implement DirectCall transport
- [ ] Test harness for in-process modification testing
- [ ] Integration tests for all 4 modifications in sync mode

### Phase 5: Trip Type Policies
- [ ] Define TripPolicy records
- [ ] Migrate scattered TripCategory pattern matching

### Phase 6: Full Flow Sync Mode
- [ ] Extend sync transport for main flow phases
- [ ] End-to-end ride flow testable in-process

### Phase 7: Thin Shell Transition
- [ ] rider-app-v2 fully functional
- [ ] driver-app-v2 fully functional
- [ ] Deprecate old rider-app/driver-app flow modules

## Decisions Log
- D1: Use records-of-functions (ModificationRecipe) instead of typeclasses for recipes. Reason: simpler, no orphan instances, easier to register dynamically.
- D2: No Value anywhere. All types are strongly typed or parameterized. Beckn message types carry real domain types.
- D3: New code goes in completely separate folders (rider-app-v2, driver-app-v2) to avoid conflicts with existing code during multi-day refactor.
- D4: Transport abstraction uses typeclass (FlowTransport) since it's a genuine polymorphism point — same code, different runtime behavior.
- D5: RideHailingFlow is parameterized by real domain types from existing codebase (Booking, Ride, etc.). No new parallel type hierarchies.
- D6: Tests use REAL domain types and live in hunit-tests/ which depends on rider-app + driver-app. No fake SimpleXxx types in mobility-flows.
- D7: Don't git add or compile without user asking.
- D8: No Maybe hacks for context fields. Use proper per-phase types or let transport instances carry their own context internally.
- D9: mobility-flows defines the flow structure (state machine, typed messages, RideHailingFlow record). Handler implementations are the EXISTING functions in Domain.Action.UI.* and Domain.Action.Beckn.*. The framework wraps them, doesn't replace them.

## Postponed Items
- ~P1~: RESOLVED. cabal.project updated, mobility-flows builds.
- P2: ModificationRecipe needs concrete monad constraint — depends on what shared constraints rider-app and driver-app both satisfy. Investigate in Phase 2.
- P3: rider-app-v2 and driver-app-v2 package.yaml files — create when we start wiring in Phase 3.
- P4: End-to-end sync test — will live in hunit-tests/, wire real handlers. Needs studying existing handler signatures first.

## Blockers
- ~B1~: RESOLVED. Domain type enums (BookingStatus, RideStatus, TripCategory) already live in beckn-spec. mobility-flows depends on beckn-spec + mobility-core. Full record types (Booking, Ride) stay in apps — flow is parameterized by them.
- B2: OPEN. Existing handler signatures are complex (e.g., `search :: SearchRequestFlow m r => Id Person -> SearchReq -> Maybe Version -> ... -> m SearchRes`) with many parameters beyond the core domain types. Need to understand how to wrap them in the RideHailingFlow record shape. May need adapter functions in the wiring layer.

## Build Notes
- New packages must be `git add`ed before `nix develop .#backend` can see them (nix flake uses git-tracked tree)
- `DuplicateRecordFields` causes `-Wambiguous-fields` — avoid same field names across types in the same module
- Empty modules need no import (just `module Foo where` is fine with `-Werror`)
- `handle` is exported by Kernel.Prelude — don't use it as a field name

## Key File Locations
- Design doc: `Backend/.cursor/docs/domain-building-blocks.md`
- Shared lib: `Backend/lib/mobility-flows/`
- Rider thin shell: `Backend/app/rider-platform/rider-app-v2/`
- Driver thin shell: `Backend/app/provider-platform/driver-app-v2/`
- This tracker: `memory/building-blocks-refactor.md` + `lib/mobility-flows/REFACTOR-TRACKER.md`
