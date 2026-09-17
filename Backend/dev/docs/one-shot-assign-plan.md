# One-Shot Assignment — Implementation Plan

> **Post-review hardening (2026-09-12):** eligibility gate additionally excludes
> online-payment merchants and pickup-zone (gate) searches; the internal payload carries
> commission/paymentCharge/paymentChargeBearer (set at BAP booking build); the BAP quote
> is built with `selectedOfferId = Nothing` (offers are NOT applied on one-shot rides —
> discount round-trip is a phase-2 item); the BAP endpoint uses a WAITING lock
> (`withWaitOnLockRedisWithExpiry`, lock TTL 60s / wait 30s — a racing duplicate blocks
> until the first attempt finishes, then lands on the idempotency ladder; note the BPP's
> `withShortRetry` only retries connection errors/503, never API errors), does all
> ride-existence checks via `runInMasterDbAndRedis` (read-your-own-write), tolerates
> late failures after `QRide.createRide` (ride exists ⇒ Success even if a post-create
> side effect threw), and verifies the ride exists before answering Success; the
> one-shot flag cache TTL is 120s (~4 min worst-case with the two cache layers
> composed). Rollout precondition: BPP and BAP `merchant.onlinePayment` flags must
> agree, or the BPP gate admits rides the BAP guard rejects. Known phase-1 gaps, by
> design: enableOtpLessRide/enableFrequentLocationUpdates, nightSafetyCheck/consent,
> BPP booking.maxEstimatedDistance/displayBookingId are not carried (all legal-Nothing
> states); rider name goes out on every VANP select (not one-shot-gated).
>
> **Status (2026-09-12): implemented** per the file touch list below, with two deltas:
> the one-shot action is passed into `acceptDynamicOfferDriverRequest` as a
> `Maybe (DriverQuote -> m ())` callback from `respondQuote` (keeps the accept function
> polymorphic; silent-assign/allocator passes `Nothing`), and the BAP-side booking
> creation reuses `SConfirm.confirm` wholesale via a `mbOneShotBppBookingId` field on
> `DConfirmReq` instead of only parameterizing `buildBooking`.

Fast path for trusted (value-add NP) BAPs with auto-assign: when a driver accepts a dynamic-offer
search request, the BPP creates the booking + ride + assignment in one synchronous sequence and
informs the BAP through a single internal API call. The BAP then creates quote + booking + ride +
notifications in one go. The Beckn `on_select → init → on_init → confirm → on_confirm` relay is
skipped entirely for these requests. No Beckn payload changes anywhere — other BAPs/BPPs are
untouched by construction.

## Ground rules (from review)

1. No changes to `on_select` or any Beckn ACL payloads — the callback is a BAP internal API
   (`CallBAPInternal` pattern, like `rideSearchExpired`).
2. No double status writes in the sync flow: rows are built in their **final** state and written
   once. The only deliberate transition kept is `SearchTry ACTIVE → COMPLETED` (it is the
   cross-driver mutual-exclusion gate, not a cosmetic stage).
3. One-shot and legacy flows share code: the work is extracting existing logic into parameterized
   shared functions and adding thin orchestrators. No logic is copied. Acceptance check: a future
   fix in booking build / ride init / ride-assigned handling must land in both flows automatically.

## Prerequisite already in production (verified)

For value-add NPs, `select` already carries the customer phone
(BAP `Domain/Action/UI/Select.hs:258` → `Beckn/ACL/Select.hs` `tfCustomer`), and the BPP already
resolves/creates `RiderDetails` and stores `riderId` on the SearchRequest at select time
(BPP `Domain/Action/Beckn/Select.hs:122-131`, `DSR.riderId = riderId`). So the BPP has everything
it needs at accept time except the rider display name (see §BPP-5).

---

## Config

### C1. BPP: per-BAP switch on `ValueAddNP`
- `spec/Storage/configs.yaml` (`ValueAddNP`, line ~161): add `enableOneShotAssign: Maybe Bool`.
- Migration (`dev/migrations/dynamic-offer-driver-app/`):
  `ALTER TABLE atlas_driver_offer_bpp.value_add_np ADD COLUMN enable_one_shot_assign boolean;`
- `src/Storage/CachedQueries/ValueAddNP.hs`: add `isOneShotAssignEnabled :: Text -> m Bool`
  (same cache as `isValueAddNP`, reading the new column; cache invalidation identical).
- Constraint: only enable for the subscriber whose internal URL is configured in
  `appBackendBapInternal` (the internal client supports a single BAP URL, same as
  `rideSearchExpired`).

### C2. BAP: no gate needed
The BAP side is passive — the internal endpoint works regardless of which BPP calls it, and when
the BPP takes the fast path it simply never sends `on_select`, so the BAP's auto-assign machinery
never fires. No RiderConfig flag required for correctness. (Optional later: a kill switch that
makes the internal endpoint return an error, which cleanly forces the BPP fallback/cancel path.)

---

## BPP changes (dynamic-offer-driver-app)

### BPP-1. Extract `buildBooking` from Init
`Domain/Action/Beckn/Init.hs` — `buildBooking` is a `where`-local function of the handler
(~line 221). Move it to a shared module (suggested: `SharedLogic/Booking.hs`, which already hosts
booking helpers) with one new parameter `initialStatus :: DRB.BookingStatus`.
- Init handler passes `DRB.NEW` (behavior unchanged).
- One-shot passes `DRB.TRIP_ASSIGNED` and sets `riderId`/`riderName` at build time (today these
  are patched in at confirm by `updateBookingDetails` — a double write we avoid).

### BPP-2. `initializeRide` gets a `bookingPreAssigned :: Bool` param
`SharedLogic/Ride.hs:103`. When `True`, skip `QRB.updateStatus booking.id TRIP_ASSIGNED`
(line ~187) — the booking row was already created in final state. Every existing caller passes
`False`. Nothing else changes: OTP resolution, ride/rideDetails creation, onRide flags + lock
release, LTS registration, driver FCM, forks — all shared as-is.

### BPP-3. `buildDriverQuote` gets an `initialStatus` param
`Domain/Action/UI/DriverAcceptOffer.hs:223`. Legacy passes `Active`; one-shot passes `Inactive`
(the winner's quote would otherwise be flipped Active→Inactive microseconds later by
`deactivateExistingQuotes`). `deactivateExistingQuotes` itself is unchanged (bulk set is
idempotent for the winner; losers still get pulled + `CLEARED_FARE`).

### BPP-4. Branch point in `acceptDynamicOfferDriverRequest`
`Domain/Action/UI/DriverAcceptOffer.hs` — after `QDrQt.create` (~line 199), replace the
unconditional `sendDriverOffer` (~line 205) with:

```
oneShotEligible <- andM
  [ pure (searchReq.autoAssignEnabled == Just True)
  , pure (isJust searchReq.riderId)                        -- phone arrived at select
  , pure (searchTry.tripCategory == OneWay OneWayOnDemandDynamicOffer)
  , pure (not searchReq.isScheduled)                       -- phase-1 exclusion
  , pure (not isTierUpgrade)                               -- same condition used for the
                                                           -- UPGRADE_TO_CAB on_select tag
  , CQVAN.isOneShotAssignEnabled searchReq.bapId
  ]
if oneShotEligible then oneShotAssign ... else sendDriverOffer ...   -- legacy path untouched
```

All existing validations before this point (locks, expiry, active-quote, quote-limit,
extra-fee bounds, `validateSearchTryActive`) run unchanged and cover both branches.

### BPP-5. Rider name on SearchRequest (small, for driver-app UX parity)
Booking.riderName is shown to the driver; today it arrives in Beckn `confirm`. Add nullable
`riderName` to BPP `SearchRequest` (spec + migration), populate it in the select handler next to
`riderId` (BAP sends it via the already-existing `fulfillment.customer.person` field — a one-line
addition in BAP `Beckn/ACL/Select.hs` `tfCustomer`, gated on `isValueAddNP`, and the mirrored
parse in BPP select ACL). Same PII class as `booking.rider_name`, which is already stored
plaintext. If deferred, one-shot bookings show no name — functional but worse UX; recommend not
deferring.

### BPP-6. New orchestrator `SharedLogic/OneShotAssign.hs`
Runs synchronously inside the driver's respond request (same latency class as today's confirm
fork, since it is the same work):

1. `Redis.tryLockRedis (mkCancelSearchInitLockKey transactionId) 30` — exactly what Beckn init
   does; failure ⇒ concurrent cancel-search ⇒ abort accept with `SearchRequestExpired`-class
   error (driver sees "ride unavailable", same as losing today's races).
2. Load `RiderDetails` via `searchReq.riderId`.
3. `booking <- buildBooking … TRIP_ASSIGNED` (BPP-1) — riderId, riderName, exophone, fare params
   from the just-created DriverQuote.
4. `QST.updateStatus DST.COMPLETED searchTry.id` — the assignment gate (kept as a transition).
5. `QRB.createBooking booking` — single write, final state.
6. `initializeRide … bookingPreAssigned=True` (BPP-2) — ride + rideDetails + OTP + LTS +
   driver FCM + lock releases, unchanged.
7. `deactivateExistingQuotes` — losers pulled, unchanged.
8. `fork "one-shot assign callback"`:
   `withShortRetry $ CallBAPInternal.oneShotAssign apiKey url payload`.
   On terminal failure: cancel ride + booking via the existing cancel machinery **with BAP
   notification suppressed** (the BAP has no booking to receive `on_cancel`; add a
   `notifyBAP :: Bool` guard to `SBooking.cancelBooking`'s notify fork), free the driver, then
   `CallBAPInternal.rideSearchExpired` so the customer's search shows expired.
9. Release the cancel-search lock; metrics (`one_shot_assign_success/failure` counters).

### BPP-7. Internal client `SharedLogic/CallBAPInternal.hs`
Add `OneShotAssignAPI` (`internal/oneShotAssign`, `Header "token"`, POST) + `OneShotAssignReq`.
Payload = everything the BAP needs to build quote, booking, and its `BookingDetails` without a
single follow-up call:

- correlation: `transactionId`, `bppEstimateId` (estimate the customer selected), `bppQuoteId`
  (DriverQuote id), `bppBookingId`, `bppRideId`
- driver: name, mobile + country code, alternate number, rating, registeredAt, image url,
  `isDriverBirthDay`, tracking url (Nothing for now)
- vehicle: number, color, model, variant, service tier + name, age
- ride: `otp`, `isSafetyPlus`, `isAlreadyFav`/`favCount`, `previousRideEndPos`
- money: currency, `estimatedFare`, fare breakup list (`title`, `amount` — the same components
  `rideAssignedCommon`/on_init expose today), `validTill`, `specialLocationTag`
- phase-1 constants: `driverAccountId = Nothing`, `isFreeRide = False` (cash/postpaid only)

Source of truth for the field list is BAP `RideAssignedReq`/`BookingDetails`
(`Domain/Action/Beckn/Common.hs:169-202`) plus `SConfirm.buildBooking` inputs.

---

## BAP changes (rider-app)

### BAP-1. Shared quote builder extraction
`Domain/Action/Beckn/OnSelect.hs` `buildSelectedQuote` (~169-225) builds Quote + DriverOffer from
the on_select DTO. Extract the core (given driver-offer fields, build `Quote` +
`DriverOfferDetails`) into `SharedLogic/` so on_select and one-shot share it. One-shot creates the
single winning quote (DriverOffer `ACTIVE` — its normal terminal state; no later flip exists in
this flow, so no double write).

### BAP-2. `SConfirm.buildBooking` parameterization
`SharedLogic/Confirm.hs:373`: add `initialStatus :: DRB.BookingStatus` and
`mbBppBookingId :: Maybe (Id DRB.BPPBooking)`. Existing callers pass `NEW` / `Nothing`. One-shot
passes `TRIP_ASSIGNED` / `Just bppBookingId` — so neither the NEW→TRIP_ASSIGNED update nor
`updateBPPBookingIdAndProviderUrl` runs later.

### BAP-3. `rideAssignedReqHandler` / `assignRideUpdate` pre-persisted mode
`Domain/Action/Beckn/Common.hs`: add `bookingPrePersisted :: Bool` to `ValidatedRideAssignedReq`
(False in all existing construction sites). In `assignRideUpdate` (~545-716), when True skip
exactly two writes: `QRBE.updateBPPBookingIdAndProviderUrl` and
`QRB.updateStatus … TRIP_ASSIGNED`. Everything else — `buildRide`, `QRide.createRide`, fare
breakups persist, `QPFS.clearCache`, customer FCM, `callTrack`, reminder-scheduler jobs, all
forks — is the one shared implementation.

### BAP-4. New internal endpoint
- `API/Internal/OneShotAssign.hs` (route `internal/oneShotAssign`, token header,
  `Domain.OneShotAssignReq`, mirrors `RideSearchExpired.hs`) and wire into `API/Internal.hs`
  (import + `:<|>` in both the API type and handler).
- `Domain/Action/Internal/OneShotAssign.hs` handler, designed **resumable** so BPP retries are
  safe after a partial failure:
  1. apiKey check (`internalAPIKey` pattern).
  2. `Redis.whenWithLockRedis ("Customer:OneShotAssign:TxnId-" <> transactionId) 60`
     + `SConfirm.tryInitTriggerLock searchRequestId` (excludes a concurrent manual confirm).
  3. Idempotency ladder: ride by `bppRideId` exists ⇒ `Success`; else booking by `bppBookingId`
     exists ⇒ resume at step 7; else continue.
  4. Load searchRequest (by transactionId), estimate (by `bppEstimateId`), person; guard estimate
     status is assignable (`DRIVER_QUOTE_REQUESTED`/`GOT_DRIVER_QUOTE`).
  5. Build + persist Quote/DriverOffer (BAP-1).
  6. Build + persist Booking as `TRIP_ASSIGNED` with `bppBookingId` (BAP-2); persist fare
     breakups (`INITIAL_BOOKING` + `BOOKING`, the on_init logic — reuse its builder);
     `QEstimate.updateStatus COMPLETED` (single transition).
  7. Construct `ValidatedRideAssignedReq` (`bookingPrePersisted = True`,
     `isSynchronousOnUpdateProcessing = True` — we are already inside one request) and call
     `DCommon.rideAssignedReqHandler` (BAP-3). This yields ride row, customer notification,
     `callTrack`, reminders — no intermediate state ever visible to the polling client
     (`selectResult` already returns `bookingIdV2` for `TRIP_ASSIGNED`; **zero app changes**).

### BAP-5. Select ACL name addition (pairs with BPP-5)
`Beckn/ACL/Select.hs` `tfCustomer`: also set `customerPerson = Just (Spec.Person {personName})`
when `isValueAddNP` (name from the person record already loaded in
`Domain/Action/UI/Select.hs`). Field already exists in the spec type; non-VANP payloads unchanged.

---

## Explicitly out of scope (phase 1 — all fall back to legacy per-request)
Scheduled rides, cab-upgrade offers, ambulance/delivery, `requiresPaymentBeforeConfirm`/online
payment (Stripe), multimodal. The eligibility check in BPP-4 is the single gate; each exclusion
is one predicate.

## Failure matrix

| Failure | Handling |
|---|---|
| Cancel-search races accept | `cancelSearchInit` lock (BPP-6.1) — same guarantee as legacy init |
| Second driver accepts | `validateSearchTryActive` + SearchTry `COMPLETED` at step 4 — unchanged |
| Internal call fails (retries exhausted) | BPP cancels ride+booking (BAP-notify suppressed), frees driver, `rideSearchExpired` → customer sees search expired |
| BAP crashes mid-handler | BPP retry hits the idempotency ladder (BAP-4.3) and resumes |
| Duplicate delivery | Same ladder ⇒ `Success` no-op |
| Manual confirm racing the callback | `tryInitTriggerLock` — loser errors, winner proceeds |

## Rollout & verification
1. Ship with `enable_one_shot_assign` NULL everywhere; enable for the on-us subscriber in one
   city.
2. Metric: the accept→assigned ClickHouse decomposition — one-shot rows collapse
   `accept_to_bpp_booking` and `bpp_ride_to_bap_ride` to ~0; expected tp50 0.3–0.7s (from ~2s),
   tp90 ~1–1.5s (from 6–10s), tp95 ~1.5–2.5s (from 10–14s).
3. Check downstream consumers that assume BAP bookings pass through `NEW` (CDC/analytics) — the
   one-shot booking is born `TRIP_ASSIGNED`.
4. Integration test via the testing framework: dynamic-offer happy path with flag on/off,
   cancel-search race, callback-failure cancel path.

## File touch list

| # | File | Change |
|---|---|---|
| 1 | BPP `spec/Storage/configs.yaml` | `ValueAddNP.enableOneShotAssign`; `SearchRequest.riderName` |
| 2 | BPP migration | two ALTER TABLEs |
| 3 | BPP `Storage/CachedQueries/ValueAddNP.hs` | `isOneShotAssignEnabled` |
| 4 | BPP `Domain/Action/Beckn/Init.hs` | extract `buildBooking` → SharedLogic (status param) |
| 5 | BPP `SharedLogic/Ride.hs` | `initializeRide` `bookingPreAssigned` param |
| 6 | BPP `Domain/Action/UI/DriverAcceptOffer.hs` | quote status param + eligibility branch |
| 7 | BPP `SharedLogic/OneShotAssign.hs` (new) | orchestrator |
| 8 | BPP `SharedLogic/CallBAPInternal.hs` | `oneShotAssign` client + req type |
| 9 | BPP `SharedLogic/Booking.hs` | `notifyBAP` guard on cancel |
| 10 | BPP `Domain/Action/Beckn/Select.hs` + Select ACL | parse + store `riderName` |
| 11 | BAP `Beckn/ACL/Select.hs`, `Domain/Action/UI/Select.hs` | send `customerPerson.personName` |
| 12 | BAP `Domain/Action/Beckn/OnSelect.hs` | extract quote builder → SharedLogic |
| 13 | BAP `SharedLogic/Confirm.hs` | `buildBooking` status/bppBookingId params |
| 14 | BAP `Domain/Action/Beckn/Common.hs` | `bookingPrePersisted` mode |
| 15 | BAP `API/Internal/OneShotAssign.hs` (new) + `API/Internal.hs` | endpoint wiring |
| 16 | BAP `Domain/Action/Internal/OneShotAssign.hs` (new) | handler |
