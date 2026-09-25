# EasyBooking RideOtp — Design Doc

> **Status:** Design / implementation plan for review. Describes changes **not yet written** at the
> time this doc was authored. Every file path and line number below was verified against the working
> tree when written; re-verify before implementing if the tree has since moved.

## Summary

EasyBooking (destination-less search category, like Rental but priced on the Regular/Progressive fare
formula) currently ships supporting **only** the static-offer dispatch path. This change makes
`EasyBooking RideOtp` a first-class flow, so when a pickup point sits inside a special zone (e.g. an
airport geofence), the BPP offers a quote that needs **no upfront driver assignment** and generates a
pickup OTP at confirm time — whichever queued driver arrives enters that OTP to create + start the
ride (`otpRideCreateAndStart`).

This is the deliberate follow-up promised by the `-- RideOtp deliberately deferred to a follow-up PR`
comments left in the original EasyBooking PR (#15753).

### Why this is small
- `EasyBookingMode = TripMode = RideOtp | OnDemandStaticOffer` — EasyBooking already reuses Rental's
  exact mode type, so `EasyBooking RideOtp` is a **legal, constructible value today**. No new type,
  no new constructor, no schema/YAML/codegen change.
- The whole runtime path a RideOtp booking travels — gate resolution, Redis demand/supply, the
  OTP-claim endpoint, `handleRideOtpFlow`, the fare-policy/quote path — is **generic over
  `TripCategory`** and needs zero changes.
- What's missing is a small set of hand-picked **pattern-match sites that enumerate categories
  explicitly** instead of falling through a wildcard, plus **one wire-encoding function**.

**Every touched file is hand-written Haskell (not `src-read-only/` generated code).**

---

## Changes

### Layer 1 — Shared lib (`Backend/lib/beckn-spec/src/`)

**1. `Domain/Types/Trip.hs` — `isRideOtpTrip` (~L408)** · *add case*
```haskell
  isRideOtpTrip (Rental RideOtp) = True
+ isRideOtpTrip (EasyBooking RideOtp) = True
  isRideOtpTrip _ = False
```
Most load-bearing line — this predicate has no catch-all for EasyBooking today. Drives:
ChangeServiceTier / AddBaggage gating (rider-app `Update.hs`), OTP-valid-ride-count incentive
(`EndRide.hs:670`), special-zone payout scheduling (`StartRide.hs:270`), and RideFlowDebug flow
detection.

**2. `BecknV2/OnDemand/Utils/Common.hs` — `tripCategoryToFulfillmentType` (L185–196)** · *add case*
```haskell
  r@(Rental RideOtp) -> show r            -- "Rental_RideOtp"
  Rental _           -> show Enums.RENTAL
  ...
+ r@(EasyBooking RideOtp) -> show r        -- "EasyBooking_RideOtp"
  EasyBooking _           -> show Enums.RENTAL
```
FulfillmentType is a fixed Beckn enum with no EasyBooking slot. Without the as-pattern, **both
EasyBooking modes serialize to `"RENTAL"`** and are indistinguishable on decode — the exact lossy
case already special-cased for Rental just above. Decode side (`fulfillmentTypeToTripCategory`) needs
no change: it already tries `readMaybe @TripCategory` first, which parses `"EasyBooking_RideOtp"` once
encode emits it.

### Layer 2 — BPP (`dynamic-offer-driver-app/Main/src/`)

**3. `Domain/Action/Beckn/Search.hs` — `getPossibleTripOption`, `Nothing` branch (L1061–1063)** · *extend list*
```haskell
  Nothing -> case dsReq.riderPreferredOption of
-   DRPO.EasyBooking -> [EasyBooking OnDemandStaticOffer]
+   DRPO.EasyBooking -> [EasyBooking OnDemandStaticOffer] <> [EasyBooking RideOtp | not isScheduled]
    _ -> [Rental OnDemandStaticOffer] <> [Rental RideOtp | not isScheduled]
```
The **live path** for EasyBooking — it never carries a `dropLocation`, so search always lands here.
Mirrors the Rental fallback on the next line.

**4. `Domain/Action/Beckn/Search.hs` — `localBundleForPreference` (L1032–1035)** · *extend list*
```haskell
- DRPO.EasyBooking -> [EasyBooking OnDemandStaticOffer]
+ DRPO.EasyBooking -> [EasyBooking OnDemandStaticOffer] <> [EasyBooking RideOtp | not isScheduled]
```
Realistically unreached for EasyBooking today (destination-less searches don't take this branch), but
kept symmetric with the `DRPO.Rental` arm above it to avoid a latent trap.

**5. `Domain/Action/Beckn/Confirm.hs` — `validateRequest` (L331)** · *add case*
```haskell
+ EasyBooking RideOtp            -> getRideOtpQuoteDetails booking transporter
  EasyBooking OnDemandStaticOffer -> getStaticQuoteDetails booking transporter
  _ -> throwError . InvalidRequest $ "UNSUPPORTED TYPE CATEGORY" ...
```
Without this, `EasyBooking RideOtp` hits the catch-all and fails confirm with **"UNSUPPORTED TYPE
CATEGORY"**. `handleRideOtpFlow` (dispatched from `handler` at L127) needs no change — it's generic
over `ValidatedQuote`.

**6. `Storage/Queries/Transformers/Booking.hs` — `getBookingTypeFromTripCategory` (L69–74)** · *add case*
```haskell
  Rental RideOtp      -> SpecialZoneBooking
+ EasyBooking RideOtp -> SpecialZoneBooking
```
Runs on **every booking write** (via the `ToTType'` instance in `OrphanInstances.Booking`). Without it
the booking persists as `NormalBooking` instead of `SpecialZoneBooking`, which the OTP-claim lookup
relies on.

### Layer 3 — BAP (`rider-app/Main/src/`)

**7. `Beckn/OnDemand/Transformer/OnSearch.hs` — tripCategory override (L110–116)** · *preserve mode*
```haskell
  let tripCategory =
    if "ON_DEMAND_EASY_BOOKING" `elem` itemCategoryIds
-     then EasyBooking OnDemandStaticOffer          -- hardcoded
+     then case fulfillmentTripCategory of
+            EasyBooking mode -> EasyBooking mode    -- keep RideOtp vs static
+            _ -> EasyBooking OnDemandStaticOffer    -- fallback
      else fulfillmentTripCategory
```
Today this hardcodes the mode to static-offer. Change it to trust the mode the BPP decoded — which is
`EasyBooking RideOtp` once change #2 emits the on-us wire string. The category-id override still
guards against the lossy-`RENTAL` misclassification it was originally added for.

**8. `Beckn/OnDemand/Transformer/OnSearch.hs` — `quoteDetails_` dispatch (L191)** · *widen case*
```haskell
- EasyBooking OnDemandStaticOffer -> pure $ EasyBookingDetails (EasyBookingQuoteDetails {quoteId = quoteOrEstId_})
+ EasyBooking _ -> pure $ EasyBookingDetails (EasyBookingQuoteDetails {quoteId = quoteOrEstId_})
```
Both modes build the identical quote-details shape (a bare quote id; price rides generically via
`quoteBreakupList_`). Collapsing the two EasyBooking arms into one `EasyBooking _` is simpler than a
near-duplicate line.

**9 & 10. `Storage/Queries/RideExtra.hs` — `isOtpRideOrRentalIntercityRide` (L404–409) and
`filterBookingsWithConditions` (L485–488)** · *add case*
```haskell
  DRB.RentalDetails _    -> True
  DRB.InterCityDetails _ -> True
+ DRB.EasyBookingDetails _ -> True
```
Between confirm and the driver entering the OTP, the booking has no assigned ride. These predicates
keep it visible / in the active-bookings list. **Confirmed needed** — `DRB.EasyBookingDetails` is its
own constructor (already pattern-matched elsewhere in `Common.hs`), so it is *not* auto-covered by the
`RentalDetails` lines.

---

## Explicitly out of scope (verified generic — no change needed)

- `otpRideCreateAndStart` / `otpRideCreate` — resolves booking by (city, OTP, expiry), never by category
- `findBookingBySpecialZoneOTP(AndCity)` — no trip-category filter
- `SharedLogic/SpecialZoneDriverDemand.hs` — Redis counters keyed by (gateId, vehicleVariant)
- `handleRideOtpFlow` (Confirm.hs) — generic over `ValidatedQuote`
- `Beckn/ACL/OnConfirm.hs` / `OnCancel.hs` — specialZoneOtp plumbing branches on driver-contact presence, not category
- Fare-policy / quote path (`getAllFareProducts` → `processPolicy` → `tripCategoryToPricingPolicy`)
- `EasyBookingSearchReq` — no new field; RideOtp is a BPP-side decision, never a rider request
- YAML / NammaDSL / `src-read-only/` — untouched

---

## Deployment prerequisite (data, not code)

A `fare_product` row for `(specialZoneArea, EasyBooking RideOtp, vehicleServiceTier)` must exist in
each target special zone. **This row is the actual on/off switch** — without it,
`getAllFarePoliciesProduct` resolves zero fare policies for the RideOtp candidate and no quote is ever
produced, even with all code changes merged.

---

## Testing

1. **Build under `-Werror`:** `cabal build lib-beckn-spec dynamic-offer-driver-app rider-app` —
   missing-case warnings fail the build, so the compiler confirms exhaustiveness and flags any site we
   missed.
2. **Search** from a lat/long inside a configured special zone (FareProduct row seeded) → confirm
   `on_search` returns an `EasyBooking RideOtp` quote alongside the static-offer one.
3. **Confirm** that quote → verify Booking gets `bookingType = SpecialZoneBooking`,
   `specialZoneOtpCode` set, no ride/driver assigned yet.
4. **Claim** via driver-app `otpRide` endpoint from a different driver account → verify it creates
   *and* starts the ride in one call.
5. **Post-conditions:** AddBaggage / ChangeServiceTier work pre-ride; RideFlowDebug classifies it as
   `RideOtpFlow`.

---

## Related docs

- `05-beckn-protocol-flow.md` — BAP/BPP protocol step file paths
- `06-ride-flow.md` — end-to-end ride lifecycle
- `16-status-definitions.md` — booking/ride state machines (`SpecialZoneBooking`, `NormalBooking`)
