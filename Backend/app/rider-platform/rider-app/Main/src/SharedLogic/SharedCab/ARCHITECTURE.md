# Shared cab: rider-app architecture

Facts read from the code on branch `backend/feat/shared-cab`. Paths are relative to `Backend/app/rider-platform/rider-app/Main/src/` unless stated.

## 1. Module map

Public facade: `SharedLogic/SharedCab.hs` re-exports the tier predicates (`isSharedCabBooking`, `isSharedCabAgency`, `sharedCabFareTiers`), `filterCabLegs`, `recordSearch`, `sharedCabBoardStops`, walk-up (`walkUpVehicleData`, `routeServiceability`, `cabRouteStations`), confirm (`onBookingCreated`, `confirmHook`), leg state (`getSharedCabLegState`, `noBookingLegState`), boarding (`tryBoardSharedCab`, `boardingResponse`), cancel (`guardRiderCancel`, `dropRider`, `CancelStage`) and refund (`Refund`, `cancelRefund`, `readRefundDecision`, `refundAmounts`, `refundWithheld`). Outside callers are meant to import it; API handlers, scheduler jobs and generated code import sub-modules directly.

| Module (`SharedLogic/SharedCab/`) | Owns |
|---|---|
| `Keys.hs` | Every Redis key and lock name (unprefixed). |
| `Config.hs` | `SharedCabTunables`, defaults, `getTunables` (rider_config via ConfigPilot). |
| `Enabled.hs` | Per-city switch `sharedCabEnabled`, `requireSharedCabEnabled`, `tickChainContinues`, `cabItineraryShown`. |
| `Plate.hs` | `canonicalisePlate`: uppercase, strip whitespace, `-`, `.`. |
| `SessionState.hs` | Pure session model: `Session`, transitions (`newSession`, `switchRoute`, `queueRoute`, `pauseSession`, `resumeSession`, `endSession`, `setWalkup`, `fillCab`), `planSelect`, `expiryAction`, `sessionFromTrip`. |
| `Session.hs` | Effectful session ops under the plate lock: `selectRoute`, `applyQueuedRoute`, `endRoute`, `expire`, `pause`, `resume`, `setWalkupCount`, `markCabFull`, `getSession` (flush recovery), `activeSessionsOnRoute`, `dropStrandedRider`, `filterCabLegs`, cab driver info. |
| `SessionView.hs` | Driver-contract request/row types; re-exports `Domain.Types.SharedCabSession` (beckn-spec). |
| `LtsAttach.hs` | Attach/detach the cab's route in LTS (`attach`, `detach`, `withAttach`); each LTS step bounded to 6 s (`ltsStepSec`). |
| `Booking.hs` | `shared` (cross-app master cell), `withBookingLock`, `tryWithBookingLock`, `markDropped`, seat counting (`liveSeatsOnVehicle`, `boardedSeatsOnVehicle`, `partySizes`), `recordRiderFix`, `recordCancelReason`, `reseatOnWaybill`. |
| `Allocation.hs` | Tick entry points: `allocationPass`, `runSharedCabAllocationTick`, `triggerSharedCabAllocation`, `releaseSharedCabAllocation`, `skipSharedCabAllocation`, `releaseUnboarded`; `allocationPass` in three phases (`readCity`, `closeExpired`, `claimFindings`). |
| `Lts.hs` | Reads of the LTS route hash `route:{routeCode}` (`readRoutePositions`, `readRoutePosition`). |
| `BookingState.hs` | A booking's allocation state in one Redis hash: alloc record, attempts, finding-since, skipped plates. |
| `Allocation/Types.hs` | `AllocationState`, `Timer` (`NoTimer`, `StandTimer`, `MovingTimer`, `AwayTimer`, each with its deadline), `AllocationOutcome`, `outcomePolicy` (blame, attempt, driver-miss and skip-plate rules per outcome), `allocKeyTtl`, `findingTimeoutAction`, `passedStopBlame`. |
| `Allocation/Select.hs` | Pure decisions: candidate eligibility (`eligibleCandidates`, nearest first, each with a `CabStance`), `claimTimer`, `LiveBooking` and `phase` (`Finding`, `AllocatedTo plate`, `NotWaiting`), `claimable`, `closable`, `silentCab`, `movingCab`, `skippedWhileFinding`. |
| `Allocation/Claim.hs` | `attemptClaim` / `claimFirst` (plate lock then booking lock; pure `claimGate`), `claimFallbackPush`, driver FCM via `CallBPPInternal.sharedCabAllocationFCM`. |
| `Allocation/Reallocate.hs` | `closeAllocation` (the one close path: lock, `closeLocked`, then `afterClose` outside the lock), `closeLocked` (CAS plate to null, bump attempts; result `Reallocated`, `ReallocatedIntoFallback` or `AutoCancelled`), `afterClose` (event, counters, push), `cancelForNoShows`, `releaseCancelledBooking` (calls `SharedLogic.FRFSCancelEffects`, the same function the BPP cancel callback uses). |
| `Allocation/Timers.hs` | `expireTimers` (stand/moving/away timeout, lost key, silent cab), `pushTimeFallbacks`. |
| `StopProgress.hs` (+ `StopProgress/Rules.hs`) | Per-tick, per-cab: board stop passed, moving-timer arm, drop auto-end clock, off-route pause, `reachedEndAt`. Pure rules in `Rules.hs`. |
| `FindingTimeout.hs` | `cancelTimedOutFindings` (cap `maxFindingTimeoutBatch` = 25 per tick), `startRefund`. |
| `Boarding.hs` | `tryBoardSharedCab`: code resolution, proximity ladder, re-bind, degraded boarding. |
| `Degraded.hs` | Degraded-boarding marker and its expiry (`expireDegradedBoardingIfNeeded`). |
| `RateLimit.hs` | Boarding attempt limit and per-vehicle no-location boarding count. |
| `Cancel.hs` + `RefundPolicy.hs` + `RefundDecision.hs` | Every shared-cab cancel (`withSharedCabCancel`, `guardRiderCancel`, `dropRider`); pure policy (`decideCancel`, `routeRiderDrop`); refund hand-off key. |
| `RefundRetry.hs` | Retry ledger for a refund that failed to start; `decideRetryStep`. |
| `DriverAction.hs` | Driver cancel / boarded-without-code / dropped (`runDriverAction`). |
| `ConfirmHook.hs` | `confirmHook` (seed tick + immediate allocation on confirm), `onBookingCreated` (event, leg mode fix). |
| `LegState.hs` + `JourneyLeg.hs` | `SharedCabState` derivation (`deriveSharedCabState`) and the leg status the app polls (`getSharedCabLegState`). |
| `Demand.hs` | Search-demand buckets and per-stop waiting/searching counts. |
| `WalkUp.hs` | Plate/code resolution to a live session; route serviceability for typed plates. |
| `NoShow.hs` | No-show / miss counters (`record`) and `actionAfterClose`. |
| `Notify.hs` | Rider pushes (assigned, arriving, reassigned, board-any, cancelled, finding-timeout, route-change). |
| `Events.hs` | Kafka events; topic `rider_config.shared_cab_events_topic`, default `shared-cab-events`. |
| `Invariants.hs` | Read-only drift checks (`checkBooking`, `checkCab`); logs `invariant_violation`, never throws. |
| `Schedule.hs` | Job chain seeding, per-tick run claims, leases. |

## 2. Lifecycle

### 2.1 Session (`SessionStatus` ACTIVE / PAUSED / ENDED; `PauseReason` NO_LOCATION / DRIVER_OFFLINE / ABSENT / OFF_ROUTE)

State lives at `sharedcab:session:{plate}`; a version counter bumps on every write; the live trip row is `vehicle_trip`.

| Transition | Trigger | Function |
|---|---|---|
| none or ENDED to ACTIVE | driver selects a route | `Session.selectRoute` (`planSelect` returns `OpenSession`; `newSession`) |
| ACTIVE to ACTIVE, same route | repeated select | `selectRoute` (`KeepRoute`, no write) |
| ACTIVE to ACTIVE, new route | select with no riders on board, or `Force` | `Session.switchTo` (`switchRoute`, closes old trip as ROUTE_CHANGED) |
| ACTIVE, route queued | select with riders on board and mode `AfterLastDrop` | `selectRoute` (`queueRoute`); applied by `Session.applyQueuedRoute` once no rider is on board (after a rider drop) |
| ACTIVE to ACTIVE, return route | driver ends with `RETURN` | `Session.endRoute` `StartReturn` (`returnRouteOf`: `-F` and `-R` suffix swap) |
| ACTIVE to PAUSED(NO_LOCATION) | no fresh LTS ping for 15 min | `SharedCabSessionExpiry` job, `Session.pause` |
| ACTIVE to PAUSED(OFF_ROUTE) | off the route polyline more than `offRouteMeters` for `offRouteSec` | `StopProgress.offRouteStep`, `Session.pause` |
| PAUSED to ACTIVE | driver resumes (also resets `consecutiveMisses`) | `Session.resume` |
| ACTIVE or PAUSED to ENDED | driver ends (`END`; trip END_ROUTE if `atLastStop` else END_FOR_NOW) | `Session.endRoute` -> `finish` |
| ACTIVE or PAUSED to ENDED | no ping for 60 min (trip ABANDONED, SESSION_TIMEOUT) | `SharedCabSessionExpiry` job, `Session.expire` -> `finish` |
| lost key to ACTIVE or PAUSED | Redis flush; trip row still live | `Session.getSession` (`sessionFromTrip`) |

- Every pause and end is followed, outside the plate lock, by `Allocation.releaseUnboarded` (outcome `SessionClosed`; `RouteChanged` on a return).
- `finish` drops each on-board rider (`Booking.markDropped`), saves ENDED first, closes the trip, clears driver info, detaches LTS.
- No writer of `DRIVER_OFFLINE` or `ABSENT` exists in rider-app or driver-app.
- Expiry thresholds are constants in `Scheduler/Jobs/SharedCabSessionExpiry.hs`: `pauseAfter` = 15 min, `endAfter` = 60 min (not rider_config; the file comments that they should move there). An unreadable ping is skipped until the trip is older than `endAfter`.
- Capacity: `available = capacity - walkupCount - liveSeats`. Default capacity 4 (`defaultCapacity` in `Domain/Action/UI/SharedCabInternal.hs`) unless the driver app sends one.

### 2.2 Booking and allocation

A booking is a `frfs_ticket_booking` on tier SHARED_CAB, status CONFIRMED, plus its tickets. The allocated cab is `vehicleNumber` (set by a CAS, `QFRFSTicketBooking.updateAllocatedVehicle`). No separate state column; the state is derived (section 2.3).

| Transition | Trigger | Function |
|---|---|---|
| booking created | confirm | `ConfirmHook.onBookingCreated`; `confirmHook` seeds the tick and forks an allocation pass |
| FINDING to ALLOCATED | tick (or trigger) finds a cab | `Claim.attemptClaim` via `claimFirst` |
| ALLOCATED to FINDING | alloc timer expired / key lost / cab silent | `Timers.expireTimers` -> `Reallocate.closeLocked` |
| ALLOCATED to FINDING | cab passed the board stop unboarded | `StopProgress.stepCab` -> `Allocation.releaseSharedCabAllocation` (`PassedStop blame`) |
| ALLOCATED to FINDING | driver cancels allocation | `DriverAction.runDriverAction` `DriverCancel` (outcome `DriverCancelled`) |
| ALLOCATED to FINDING | rider skips the cab | `Allocation.skipSharedCabAllocation` (`RiderSkipped`), endpoint `POST /sharedCab/booking/{id}/skip` |
| ALLOCATED to FINDING | cab full (walk-ups) | `Session.markCabFull` then `releaseUnboarded` `SeatLost` |
| ALLOCATED to FINDING | cab paused/ended/route changed | `releaseUnboarded` `SessionClosed` / `RouteChanged` |
| FINDING or ALLOCATED to BOARDED | rider enters code / proximity ok / driver "boarded without code" | `Boarding.tryBoardSharedCab`; `DriverAction` `DriverBoarded` (tickets ACTIVE to INPROGRESS) |
| FINDING or ALLOCATED to DEGRADED | unknown code with `forceCheckIn` | `Boarding.degradedBoarding` (tickets INPROGRESS, marker set, allocation key deleted) |
| BOARDED to DROPPED | driver "dropped", rider "I got down", drop auto-end, session end | `Booking.markDropped` (tickets INPROGRESS to USED) via `DriverAction`, `Cancel.dropRider`, `StopProgress.dropStep`, `Session.finish` |
| DEGRADED to DROPPED | marker expired (rider poll or sweep) | `Degraded.expireDegradedBoardingIfNeeded` |
| stranded rider to DROPPED | session gone, ticket still INPROGRESS | `Session.dropStrandedRider` (sweep) |
| to CANCELLED | rider cancel / driver cancel | `Cancel.withSharedCabCancel` (`guardRiderCancel`, `DriverAction`) |
| to CANCELLED | FINDING too long | `FindingTimeout.cancelTimedOutFindings` -> `cancelOne` |
| to CANCELLED | rider's last allowed no-show | `Reallocate.closeLocked` -> `cancelForNoShows` |

`AllocationOutcome` (`Allocation/Types.hs`), blame (`blameFor`) and attempt counting (`countsTowardAttempts`):

| Outcome | Meaning | Blame | Counts attempt |
|---|---|---|---|
| `StandTimeout` | stand timer ran out | rider | yes |
| `MovingTimeout` | moving timer ran out after cab reached stop | rider | yes |
| `AwayTimeout` | bounded wait for a stationary cab away from the stop ran out | none | yes (the cab is skipped, see below) |
| `DriverCancelled` | driver cancelled | driver | yes |
| `PassedStop b` | cab passed stop; `passedStopBlame`: fresh rider fix within `atStopRadiusM` of stop = driver, fresh fix elsewhere = rider, none/stale = none (fresh = within `ltsMaxAgeSec`) | `b` | yes |
| `SeatLost` | capacity eviction | none | yes |
| `RouteChanged` | cab changed route | none | no |
| `SessionClosed` | session paused/ended | none | no |
| `TimerLost` | alloc key vanished | none | no |
| `CabSilent` | no LTS fix for 5 x `ltsMaxAgeSec` (`silentReleaseMult`) while other cabs on the route report | none | no |
| `RiderSkipped SkipFull` | rider says cab is full | none | no |
| `RiderSkipped SkipOther` | rider skip, other reason | rider | yes |

- Counters: `NoShow.record` charges `vehicle_trip.missed_pickups` (trip from the alloc key's `vehicleTripId`) for any driver-blamed close (`DriverCancelled`, `PassedStop BlameDriver`; `countsTowardDriverMisses` has no caller outside `Types.hs`) and `person_stats.shared_cab_no_shows` for rider blame. A rider-blamed close also sets `frfs_ticket_booking.shared_cab_no_shows` + 1 (`releaseAllocatedVehicle`).
- Plates skipped for the booking: outcomes `StandTimeout`, `MovingTimeout` and `AwayTimeout` (`skipsPlateOnClose`) and rider skips add the plate to `sharedcab:skipped:{id}`; an `AwayTimeout` still counts one attempt, but because that cab is not claimed again, a lone parked cab cannot push `attempts` to `maxAttempts` and only `fallbackAfterSec` reaches the fallback; skips are ignored once fallback is reached (`skippedWhileFinding`).
- Auto-cancel: `NoShow.actionAfterClose` returns `AutoCancel` only for a rider-blamed close when the new no-show count reaches `maxNoShows`. `cancelForNoShows` sets booking and tickets CANCELLED, keeps the fare (`NoRefund`, or `NothingPaid` if no payment row), records cancel reason `NO_SHOW_CAP`.
- Fallback ("board any cab", push `notifyBoardAny`): `attempts >= maxAttempts` or `now - findingSince >= fallbackAfterSec` (`LegState.fallbackReached`); pushed once per `fallbackPushedKey`.
- Finding timeout (`findingTimeoutAction`): cancel when current FINDING stint exceeds `findingTimeoutSec`, or booking age exceeds `3 x findingTimeoutSec` (`totalAgeMult`). Refund: `decideCancel ByRider FINDING` (full unless a no-show is booked, then none), gated by payment row. Refund failure marks `RefundRetry`.
- Timer armed at claim (`claimTimerSec`): moving cab none; stationary at stop (within `atStopRadiusM`) `standTimerSec` (StandTimer); stationary away `allocationWindowSec` (AwayTimer). A stand/away timer is cleared when the cab is seen moving (`Timers.expireTimers`); the moving timer (`movingTimerSec`) is armed by `StopProgress.movingTimerStep` once the cab is within `atStopRadiusM` of the board stop with no timer running.
- Decision (user, 2026-10-16): a cab claimed under an away timer is never given the rider-blamed moving timer when it reaches the stop radius; the away timer runs out as the blameless `AwayTimeout` (`StopProgress.movingTimerStep` arms only when `runningTimerDeadline` is empty). The ARRIVING display rule is separate and unchanged (`allocationDeadline`: stand and moving timers only).
- Cancel policy (`RefundPolicy.decideCancel`): ride started (BOARDED/DEGRADED/DROPPED or ticket INPROGRESS/USED) rejected `RideStarted`; driver cancel full refund; rider cancel while ARRIVING and rider near stop (or no fresh fix) rejected `TalkToDriver`; otherwise full refund unless `shared_cab_no_shows > 0` (none).

### 2.3 Leg states (`LegState.SharedCabState`, derived by `deriveSharedCabState`, served by `JourneyLeg.getSharedCabLegState`)

| State | Derivation |
|---|---|
| FINDING | booking CONFIRMED or ticket ACTIVE, no plate, fallback not due |
| FALLBACK | as FINDING but `fallbackDue` |
| ALLOCATED | plate set, no stand or moving timer in the alloc key (a cab held under an away timer is ALLOCATED: assigned, not yet at the stop) |
| ARRIVING | plate set and the alloc record has a stand or moving timer (`allocationDeadline`, returned as `boardDeadlineSec`); an away timer is not one |
| BOARDED | ticket INPROGRESS and a non-ENDED session exists for the plate |
| DEGRADED | ticket INPROGRESS and no live session |
| DROPPED | ticket USED or EXPIRED, or feedback state |
| CANCELLED | other ticket status; booking CANCELLED, COUNTER_CANCELLED, CANCEL_INITIATED, RESCHEDULED, FAILED |

`CancelReason` (NO_SHOW_CAP, DRIVER, RIDER, SYSTEM, NO_CAB_FOUND) is read from `sharedcab:cancelreason:{id}` for CANCELLED legs. The poll also records the rider fix (ALLOCATED/ARRIVING) and runs the degraded expiry. `onlyTransitLeg`: cancelling the only non-walk leg ends the journey (`cancelJourneyIfOnlyTransitLeg`).

## 3. Locks

Both are cross-app master-cell locks, non-reentrant (`withWaitAndLock...` deadlocks on a key already held).

| Lock | Key | TTL | Taken by |
|---|---|---|---|
| Plate lock | `sharedcab:lock:{plate}` | 30 s (`lockTtlSec`) | `Session.withPlateLock` (all session ops, `attemptClaim`, `DriverAction`, `dropStrandedRider`, route-set repair) |
| Booking lock | `sharedcab:lock:booking:{bookingId}` | 10 s | `Booking.withBookingLock` (claim, close, cancel, drop, board, degraded, finding-timeout, moving-timer arm); `tryWithBookingLock` is the non-waiting variant for read paths |

- Order: plate lock first, then booking lock; never reversed. Sites: `Claim.attemptClaim`, `DriverAction`, `Session.finish` (via `markDropped`), `Session.dropStrandedRider`.
- Never call `releaseSharedCabAllocation` or a re-bind from inside `Cancel.withSharedCabCancel`'s booking lock; `cancelAction` must not take the booking lock itself.
- `activeSessionsOnRoute` repair takes the plate lock: no caller may hold it. Releases after a session write run outside the plate lock.
- City leases (separate from the two above): allocation `sharedcab:alloc:lease:{city}`, 10 x `tickSec` (`withCityTickLease`, shared by the tick, on-demand triggers and stop progress); sweep `sharedcab:degradedSweepLease:{city}`, 10 x 120 = 1200 s, owner token checked on unlock (`withSweepLease`).
- Lock retry-delay arguments (10000 booking, 25000 plate) are passed to `Redis.withWaitAndLockMasterCloudCrossAppRedis`.

## 4. Redis keys

All keys are unprefixed and written through `Booking.shared` (cross-app master cell) unless the Access column says otherwise. `T` = tunable (section 7).

| Key | Value | TTL | Writer |
|---|---|---|---|
| `sharedcab:session:{plate}` | `Session` JSON | 16 h (`sessionTtlSec`) | `Session.saveSession` |
| `sharedcab:route:{routeCode}` | set of plates (ACTIVE sessions) | 16 h on each add | `Session.saveSession`; repaired by `activeSessionsOnRoute` |
| `sharedcab:driverinfo:{plate}` | `CabDriverInfo` | 16 h | `Session.setCabDriverInfo`; cleared on `finish` |
| `sharedcab:lock:{plate}` | lock | 30 s | `Session.withPlateLock` |
| `sharedcab:lock:booking:{id}` | lock | 10 s | `Booking.withBookingLock` |
| `sharedcab:booking:{id}` | hash: `alloc` (`AllocationState`), `attempts` (Int), `findingSince` (UTCTime, absent: booking `createdAt`), `skipped` (plates) | `allocKeyTtl` = max(`findingTimeoutSec`, `standTimerSec` + `movingTimerSec` + 10), reset by every field write | `BookingState`: written by `Claim.attemptClaim` (alloc), `StopProgress.movingTimerStep` (alloc), `Timers.expireTimers` (alloc), `Reallocate.closeLocked` (attempts, findingSince, skipped); the `alloc` field is deleted on close, boarding and driver board |
| `sharedcab:fallbackpushed:{id}` | lock-style marker | `findingTimeoutSec` | `Claim.claimFallbackPush`; deleted on close while attempts < `maxAttempts`; a once-flag, so it stays outside the hash |
| `sharedcab:riderfix:{id}` | `RiderFix` | 3600 s | `Booking.recordRiderFix` (from leg-state poll) |
| `sharedcab:cancelreason:{id}` | `CancelReason` | 24 h | `Booking.recordCancelReason`; not cleared by `clearBookingState` |
| `sharedcab:degraded:{id}` | `DegradedBoarding` | `degradedTimeoutSec` | `Degraded.markDegradedBoarding` |
| `sharedcab:dropclock:{id}` | UTCTime | 2 x `autoEndAfterDropSec` | `StopProgress.dropStep`; access `Redis.withMasterRedis` (not `shared`) |
| `sharedcab:offroute:{plate}` | UTCTime | 2 x `offRouteSec` | `StopProgress.offRouteStep`; access `Redis.withMasterRedis` |
| `sharedcab:noloc:{plate}:{istDay}` | Integer counter | 2 days | `RateLimit.recordNoLocationBoarding` |
| `sharedcab:rate:board:{id}` | Integer counter | 600 s (`boardAttemptWindowSec`), set with the key | `RateLimit.enforceBoardingAttemptLimit` |
| `sharedcab:refund:{id}` | `Refund` | 120 s | `RefundDecision.setRefundDecision` (cleared in `finally`); plain `Redis` access |
| `sharedcab:refundretry:{id}` | Int (attempts) | 2 days (`refundRetryTtlSec`) | `RefundRetry.markRefundRetry`, `bumpRefundRetry` |
| `sharedcab:refundretry:city:{city}` | set of booking ids | 2 days | `RefundRetry.markRefundRetry` |
| `sharedcab:demand:{city}:{stop}:{bucket}` | set of rider ids | 1200 s (`bucketTtlSec`) | `Demand.recordSearch`; access `Redis.withMasterRedis` |
| `sharedcab:alloc:lease:{city}` | lock | 10 x `tickSec` | `Schedule.withCityTickLease` |
| `sharedcab:allocJob:{city}` | guard (live chain id) | 10 x `tickSec` | `Schedule.ensureChain` / `scheduleNext` |
| `sharedcab:allocRun:{city}` | per-tick claim | max(1, `tickSec` - 1) | `claimTickRun` |
| `sharedcab:expiryJob:{city}` | guard (live chain id) | 120 s | `ensureExpiryJob` / `scheduleNextExpiry` |
| `sharedcab:expiryTick:{city}` | per-tick claim | 55 s | `claimExpiryRun` |
| `sharedcab:degradedSweepJob:{city}` | guard (live chain id) | 240 s | `ensureDegradedSweep` / `scheduleNextSweep` |
| `sharedcab:degradedSweepRun:{city}` | per-tick claim | 115 s | `claimSweepRun` |
| `sharedcab:degradedSweepLease:{city}` | lease with owner token | 1200 s | `withSweepLease` |
| `sharedcab:refundretryRun:{city}` | per-tick claim | 115 s | `claimRefundRetryRun` |

Notes: `clearBookingState` deletes the hash and the fallback-push flag. Demand buckets are 300 s wide (`bucketSec`), 3 buckets read (`bucketCount`; `searchWindowMin` = 15). The cab's live position is read from the LTS hash `route:{routeCode}` (field = plate), not a shared-cab key.

## 5. Jobs

All three are self-rescheduling per-city scheduler jobs (`RiderJobType` in `SharedLogic/JobScheduler.hs`; handlers registered in `Scheduler/src/App.hs`). Job data: `{merchantId, merchantOperatingCityId}`.

| Job | Cadence | Guard / run / lease | Body |
|---|---|---|---|
| `SharedCabAllocationTick` | `tickSec` (default 3 s) | guard `allocJob` (10 x `tickSec`), run claim `allocRun`, lease `alloc:lease` | `claimTickRun` and `sharedCabEnabled`; under lease: `allocationPass` (expire timers, fallback pushes, claims) then `runStopProgress`; after lease `cancelTimedOutFindings`. Next tick scheduled in `finally`. Ends if disabled. |
| `SharedCabSessionExpiry` | 60 s (`expiryTickSec`) | guard `expiryJob` (120 s), run claim `expiryTick` (55 s) | reads LTS pings per live route; pauses (15 min) / ends (60 min) silent sessions; skips a route whose LTS read fails. Does not check `sharedCabEnabled`. |
| `SharedCabDegradedSweep` | 120 s (`sweepTickSec`) | guard `degradedSweepJob` (240 s), run claims `degradedSweepRun` and `refundretryRun` (115 s), lease `degradedSweepLease` | scan pass: degraded-marker expiry (`findSharedCabDegradedCandidates`) and stranded plated riders (`findSharedCabPlatedCandidates`), pages of 200, window `updatedAt` in [now - 3 x `degradedTimeoutSec`, now); refund pass: retries `startRefund`, max 5 attempts (`maxRefundRetryAttempts`), then logs `OPS-ALERT`. Chain lapses when both passes see no work; if disabled it does not reschedule. |

Chain (re)start (`Schedule.hs`): the guard holds the id of the one live chain. `ensureChain` sets it with a fresh id if absent (+ TTL) and enqueues the first job, carrying that id, only if it won; `scheduleNext` refreshes the guard and enqueues the next job with the same id. Each tick first runs `ownChain`: its id equal to the guard continues (`Mine`); an absent guard is taken back with set-if-absent (`Adopt`); another id ends the tick without rescheduling (`Foreign`). So a lapse followed by a re-seed leaves exactly one chain: the old one dies at its next tick. The per-tick run claim still drops a job the producer dispatches twice. Jobs queued before the id existed carry none and adopt like a lapsed guard.

A stalled tick (a scheduler job delivered but never run) self-heals: the expiry tick re-seeds a lost allocation chain within a minute (`watchChains`), and the old chain's reclaimed job ends on its stale chain id.
Cause: not established. The scheduler `XREADGROUP BLOCK` (10 s) being longer than hedis' 5 s request timeout was tried and ruled out as the sole cause: with `REDIS_REQUEST_NODE_TIMEOUT=15` there were no timeouts and the wedge still happened. The watchdog covers it whatever the cause (the guard lapses, the chain is re-seeded).
Chain pairs (`Schedule.chainsSeededBy`, each a set-if-absent on the guard): the allocation tick seeds the expiry chain, the expiry tick seeds the allocation chain (while a booking is live, switched on or not) and the sweep chain (while a booking, refund retry or live trip exists), and the sweep tick seeds the expiry chain; the accepted gap is the allocation and expiry jobs being lost together, which only the reclaim or new demand revives.
A job the reclaimer requeues runs one full extra pass when it finally executes (the run claim and city lease expired long before): passes are CAS-safe on the claim, so the only visible effect is a possible duplicate push.
Rollout: an old pod's chain never reads the guard, so during a mixed old/new rollout two allocation chains may run until old pods drain (a new pod that picks up an old-format job finds the guard naming its own chain and ends it); the per-tick run claim and the city tick lease keep the passes single.

| Chain | Seeded by |
|---|---|
| Allocation tick (`ensureAllocationTick`, only if `sharedCabEnabled`) | `ConfirmHook.confirmHook` (booking confirm), `SharedCabInternal` route select and resume (`seeded`) |
| Session expiry (`ensureExpiryJob`) | `Session.selectRoute` (open), `Session.resume`, `Session.getSession` rebuild |
| Degraded sweep (`ensureDegradedSweep`, only if enabled) | `Session.selectRoute` (open), `Boarding.degradedBoarding`, `RefundRetry.markRefundRetry` |

Dhall: `dhall-configs/dev/rider-app.dhall` lists `SharedCabAllocationTick`, `SharedCabSessionExpiry`, `SharedCabDegradedSweep` in the `RiderJobType` union and in `jobInfoMapx` with value `False` each. That map is `jobInfoMap :: Map Text Bool`, read by the scheduler as `isLongRunning` (when True, job status writes also go to the DB table under the Redis scheduler). It is not an enable flag: the jobs run whenever seeded and the city is enabled (section 6). The scheduler dhall configs found in the repo are `dev` only. A scheduler restart kills the chains; re-seed by a route select or a booking confirm.

## 6. Per-city switch

`Enabled.sharedCabEnabled cityId` = the city has an `IntegratedBPPConfig` for BUS / APPLICATION whose `agencyKey` is a shared-cab agency (`isSharedCabAgency`: `gtfsIdtoDomainCode agencyKey == "SHARED_CAB"`) AND `rider_config.shared_cab_enabled` is not `false` (NULL follows the data). Decision: `sharedCabEnabledDecision hasConfig opsSwitch = hasConfig && opsSwitch /= Just False`.

A suspended city (switch false, or no SHARED_CAB config) takes no new intake and drains what is in flight.

Intake refused or hidden (`requireSharedCabEnabled` throws `InvalidRequest "Shared cab is suspended in your city"`): `Session.selectRoute`, `Session.resume`, `Session.setWalkupCount`, `Boarding.tryBoardSharedCab` (so a rider mid-board is told, not sent down the bus path), `FRFSConfirm` before a shared-cab booking is created, walk-up by live plate (`WalkUp.walkUpVehicleData`). Hidden: `filterCabLegs` drops cab itineraries (`cabItineraryShown`), `findSharedCabConfig` returns Nothing so `/sharedCab/routes` is empty, the route detail 404s and typed codes resolve to nothing.

Draining continues: `SharedCabAllocationTick` keeps rescheduling while the city is on or live bookings exist (`tickChainContinues`); with the city off, `allocationPass` still expires timers and `runStopProgress` and `cancelTimedOutFindings` still run, but no claims or board-any pushes are made. `SharedCabDegradedSweep` (degraded-ride expiry, stranded riders, refund retries) is not gated and `ensureDegradedSweep` seeds regardless. `SharedCabSessionExpiry` is not gated.

## 7. Config

### 7.1 `rider_config` tunables (`Config.SharedCabTunables`; column = `shared_cab_` + snake case; NULL or no row takes the default)

| Field | Column | Default | Unit | Read by |
|---|---|---|---|---|
| `allocationWindowSec` | `shared_cab_allocation_window_sec` | 480 | s | candidate ETA cap; away-timer length |
| `atStopRadiusM` | `shared_cab_at_stop_radius_m` | 100 | m | at-stop test, moving-timer arm, passed-stop blame, drop arrival |
| `standTimerSec` | `shared_cab_stand_timer_sec` | 180 | s | stand timer; alloc key TTL |
| `movingTimerSec` | `shared_cab_moving_timer_sec` | 90 | s | moving timer; alloc key TTL |
| `maxAttempts` | `shared_cab_max_attempts` | 2 | count | fallback trigger |
| `maxNoShows` | `shared_cab_max_no_shows` | 2 | count | auto-cancel |
| `fallbackAfterSec` | `shared_cab_fallback_after_sec` | 600 | s | time-based fallback |
| `findingTimeoutSec` | `shared_cab_finding_timeout_sec` | 1200 | s | finding timeout; TTL of the fallback-push flag; floor of the booking-state hash TTL |
| `tickSec` | `shared_cab_tick_sec` | 3 | s | allocation tick cadence and its TTLs |
| `ltsMaxAgeSec` | `shared_cab_lts_max_age_sec` | 60 | s | fix freshness; rider-fix freshness; silent-cab threshold = 5 x |
| `autoEndAfterDropSec` | `shared_cab_auto_end_after_drop_sec` | 600 | s | drop auto-end |
| `degradedTimeoutSec` | `shared_cab_degraded_timeout_sec` | 3600 | s | degraded marker TTL; sweep window |
| `offRouteMeters` | `shared_cab_off_route_meters` | 300 | m | off-route pause |
| `offRouteSec` | `shared_cab_off_route_sec` | 120 | s | off-route pause |
| `boardProximityM` | `shared_cab_board_proximity_m` | 150 | m | boarding proximity; rider-near-stop cancel check |
| `boardAttemptsPer10Min` | `shared_cab_board_attempts_per10_min` | 5 | count per 600 s | boarding rate limit |
| `noLocationSpotBookingsPerVehiclePerDay` | `shared_cab_no_location_spot_bookings_per_vehicle_per_day` | 5 | count per IST day | ops-flag log threshold |
| `maxCabSpeedMps` | `shared_cab_max_cab_speed_mps` | 14 | m/s | candidate guard: straight-line distance to the stop must not exceed ETA x this + `atStopRadiusM` (`Select.etaPlausible`) |

Other `rider_config` columns: `shared_cab_enabled` (boolean, section 6), `shared_cab_events_topic` (text, default `shared-cab-events`). No `transporter_config` column is read by rider-app. Storage columns outside rider_config: `frfs_ticket_booking.shared_cab_no_shows` (default 0), `person_stats.shared_cab_no_shows` (default 0).

### 7.2 Constants (not tunable)

| Constant | Value | Where |
|---|---|---|
| Session / route set / driver info TTL | 16 h | `Session.sessionTtlSec` |
| Plate lock TTL / booking lock TTL | 30 s / 10 s | `Session`, `Booking` |
| Alloc key TTL margin | 10 s | `allocKeyTtlMarginSec` |
| Total FINDING age cap | 3 x `findingTimeoutSec` | `totalAgeMult` |
| Silent-cab release | 5 x `ltsMaxAgeSec` | `silentReleaseMult` |
| Moving speed | speed > 1.0 | `isMovingSpeed` |
| Demand bucket / count / TTL | 300 s / 3 / 1200 s | `Demand` |
| Boarding rate window | 600 s | `RateLimit.boardAttemptWindowSec` |
| No-location counter TTL | 2 days | `RateLimit` |
| Refund decision key TTL | 120 s | `RefundDecision` |
| Refund retry TTL / max attempts | 2 days / 5 | `RefundRetry` |
| Finding-timeout batch per tick | 25 | `maxFindingTimeoutBatch` |
| Sweep page size | 200 | `SharedCabDegradedSweep.pageSize` |
| Session expiry pause / end | 15 min / 60 min | `SharedCabSessionExpiry` |
| Expiry tick / sweep tick | 60 s / 120 s | `Schedule` |
| LTS step timeout | 6 s | `LtsAttach.ltsStepSec` |
| Rider-fix TTL / cancel-reason TTL | 3600 s / 24 h | `Booking` |
| Default capacity | 4 | `SharedCabInternal.defaultCapacity` |

## 8. APIs

### 8.1 Internal (driver-app calls these; prefix `/internal`, header `token` must equal rider-app `internalAPIKey`, else `AuthBlocked`). Spec `spec/API/SharedCabInternal.yaml`, handlers in `Domain/Action/UI/SharedCabInternal.hs`.

| Endpoint | Handler |
|---|---|
| `GET /internal/sharedCab/routes?agencyId&lat&lon` | `getSharedCabRoutes` (routes of the agency feed, sorted by distance from the driver) |
| `POST /internal/sharedCab/route/select` | `postSharedCabRouteSelect` (`Session.selectRoute`, seeds tick, sets driver info, releases old route's allocations, applies walk-up count; returns `affectedRiders` when a mode is needed) |
| `GET /internal/sharedCab/session?driverId&vehicleNumber` | `getSharedCabSession` (`ownSession`, `mkSessionResp`) |
| `POST /internal/sharedCab/seats` | `postSharedCabSeats` (`Session.setWalkupCount`, version CAS) |
| `POST /internal/sharedCab/route/end` | `postSharedCabRouteEnd` (`RETURN`, `CHANGE`, `END`; returns null after END) |
| `POST /internal/sharedCab/resume` | `postSharedCabResume` |
| `GET /internal/sharedCab/trips?driverId&date` | `getSharedCabTrips` |
| `POST /internal/sharedCab/booking/{bookingId}/cancel` | `postSharedCabBookingCancel` (`DriverCancel`) |
| `POST /internal/sharedCab/booking/{bookingId}/boardedWithoutCode` | `postSharedCabBookingBoardedWithoutCode` |
| `POST /internal/sharedCab/booking/{bookingId}/dropped` | `postSharedCabBookingDropped` |
| `POST /internal/sharedCab/cabFull` | `postSharedCabCabFull` (`markCabFull`, then `releaseUnboarded` `SeatLost`) |

Outbound: rider-app calls driver-app `POST /internal/sharedCabAllocationFCM` (`CallBPPInternal.sharedCabAllocationFCM`, merchant `driverOfferApiKey`) from `Claim.notifyDriverOfAllocation`, forked; failures are logged only.

### 8.2 Rider-facing (`TokenAuth`, under the UI API prefix; `spec/API/SharedCab.yaml`, handlers in `Domain/Action/UI/SharedCab.hs`)

| Endpoint | Handler |
|---|---|
| `GET /sharedCab/routes` | `getSharedCabRoutes` |
| `GET /sharedCab/routes/{routeCode}` | `getSharedCabRoute` |
| `POST /sharedCab/booking/{bookingId}/skip` | `postSharedCabBookingSkip` (`FULL` / `OTHER`; only while a cab is allocated and unboarded) |

Other entry points into the feature are inside existing multimodal and FRFS APIs: boarding (`MultimodalConfirm` calls `SharedCab.tryBoardSharedCab`), cancel (`guardRiderCancel` in `MultimodalConfirm` and `FRFSTicketService`), "I got down" (`dropRider`), walk-up vehicle data and route serviceability (`WalkUp`), search demand (`Search.hs` calls `recordSearch`), confirm (`OnConfirm` calls `confirmHook`).

## 9. Running the e2e scenarios

Location: `Backend/dev/integration-tests/hurl/shared-cab/` (in this worktree). `/Users/dhayfa/Projects/NY/shared_session` holds notes and logs only, no hurl scripts. Files: `driver-flow.hurl`, `flush-before.hurl`, `flush-after.hurl`, `rider-flow.hurl`, `boarding-flow.hurl`, `quantity-flow.hurl`, `standalone-flow.hurl`, `login.hurl`, `local.env`, `run.sh`, `seed/01..05-*.sql`, `README.md`.

The stack must run `rider-app-scheduler-exe` and `producer-exe`: with the Redis-based scheduler the producer moves due jobs from `Scheduled_Jobs_Rider{n}` to the stream the scheduler consumes, so without it no job (tick, expiry, sweep) ever fires. A scheduler restart alone does not lose the chain, because the next job sits in the Redis set.

Command (from that directory, per `README.md`): `ENV=./mylocal.env TOKEN=<rider token> REDIS_CLI="redis-cli -c -p 30001" ./run.sh [driver|flush|rider|boarding|standalone|quantity|all]` (default `all`). `ENV` defaults to `local.env`; `REPLACE_ME` values in it must be filled; `TOKEN` defaults to a login via `login.hurl` (sandbox OTP 7891). Prerequisites in the README: `hurl`, `jq`, running rider-app (port 8013 in `local.env`), Redis, Postgres, OTPRest feed, `seed/*.sql` applied, Redis config caches cleared, LTS geo config for the shared-cab routes, and a cab pinging LTS. The driver scenarios call rider-app `/internal/sharedCab/*` directly with `internal_key`.

## 10. Deploy and ops notes
- **Search needs a running cab.** A shared-cab itinerary is shown only while a cab is running its route (`filterCabLegs`, `cabItineraryShown`: an active session on the leg's route). With no cab running, the search returns the walk-only journey. That is expected, not a data or OTP fault (the late-night headway in the GTFS feed is not involved).
- **LTS deploy order.** Roll location-tracking-service (PR #262) out completely before any driver app sends `vt: SHARED_CAB` (before enabling shared cab in the driver app for a city). An LTS that knows `SHARED_CAB` writes `vehicle_type: "SHARED_CAB"` into `lts:driver_details:{driver}`; an LTS without the variant (main, or old pods in a rolling deploy) then fails every ping for that driver with `500 DESERIALIZATION_ERROR` until the record is rewritten or its 24 h TTL ends. Local repair: rewrite that record's `vehicle_type` to SEDAN with KEEPTTL.
- **Scheduler producer and a Redis failover.** The producer did not recover from a Redis cluster failover on the local stack: its timestamp keys (`RiderproducerTimestampKey{n}`) stopped advancing at the election, the stream saw no new entries, and every scheduled job (allocation tick, session expiry, degraded sweep, refunds) stayed overdue in `Scheduled_Jobs_Rider{n}` until the producer was restarted. The chains' own watchdogs are jobs and die with it. Ops signal: those timestamp keys frozen while `Scheduled_Jobs_Rider{n}` has overdue entries; remedy: restart the producer. Detail in `shared_session/scheduler-wedge-note.md`.
