# EasyBooking — new TripCategory (Rental-shaped, Progressive fare, optional end-OTP)

## Context

Today the backend has a fixed set of `TripCategory` values (`OneWay`, `Rental`, `RideShare`, `InterCity`, `CrossCity`, `Ambulance`, `Delivery`), each pointing at one specific fare-policy shape and one specific set of procedural rules (OTP requirements, night-charge basis, toll handling, etc.). The user wants a new sibling category, **EasyBooking**, that:

1. Books like a **Rental** (rider gives an estimated distance/duration package, no destination at search time — no live route calc) — because they described it as "like rental" and confirmed the destination-less shape is correct.
2. Prices like a **Regular/OneWay** ride (Progressive fare policy: base fare + per-km-beyond-base-distance, on *actual* GPS distance) instead of Rental's package/duration-based formula.
3. Makes the end-ride OTP **a real on/off flag** — user clarified: not "always off," but an actual switch — when the flag is `true`, OTP is mandatory for the ride exactly like Rental today; when `false`, the ride works without one, exactly like OneWay today. Nothing in the codebase currently supports "sometimes required" for the same category, so this needs new plumbing (detailed below), not just reading an existing helper.

This is intentionally scoped as **config + a new enum constructor**, not a new fare-calculation engine — `SharedLogic/FareCalculator.hs`'s `processFPProgressiveDetails` (the OneWay fare formula) already works on any booking regardless of trip category, because it only reads `actualDistance` (GPS-tracked, same mechanism Rental already uses since `isOdometerReadingsRequired (Rental _) = False`) and a fixed `baseDistance` config value — it never reads `estimatedDistance`. So EasyBooking needs zero changes to the fare-calculator itself.

## Key concepts, explained plainly (read this before the design table if any term below is unfamiliar)

**QuoteBased vs EstimateBased — this is about when the price becomes final.**
- *EstimateBased*: at search time you're shown a **price range** (e.g. "₹120–₹180"), not one exact number. You then go through an extra "Select" step where a specific driver's price gets locked in before confirming. This is how normal OneWay dynamic-pricing rides work today — the range exists because the price still depends on which driver picks it up, live traffic, etc.
- *QuoteBased*: at search time the backend already computes **one final, fixed price immediately** — no range, no extra step, you just confirm. This is how Rental works today.
- *Why EasyBooking is QuoteBased*: EstimateBased needs a real route (pickup → drop) to estimate a range against. Since EasyBooking has no destination at search time (destination-less, like Rental — rider just gives a distance/duration package), there's no route to estimate a range against and nothing for "Select" to lock in. It goes straight to one fixed quote, same as Rental.

**Night fare charge — EasyBooking gets this automatically, no new code needed for it.**
There are two different methods the system uses to calculate a night-time surcharge:
1. *Fixed* (used by Rental/InterCity today) — if any part of the ride overlaps night hours, a flat extra amount is added, regardless of how much of the ride was actually at night.
2. *Dynamic/overlap-based* (used by OneWay/Progressive today) — the surcharge scales with how much of the ride's time actually overlaps the configured night window; more night overlap = more charge.

A function called `isFixedNightCharge` just picks between these two: `isRentalTrip tripCategory || isInterCityTrip tripCategory`. Since EasyBooking is neither, this automatically evaluates to `False` — giving it the *dynamic* method, same as OneWay, which is correct since EasyBooking shares OneWay's Progressive fare formula. The actual rate/config (what hours count as "night," how much extra to charge) comes from the same `fare_policy` table columns every Progressive-priced category already uses (`night_shift_start`, `night_shift_end`, `fare_policy_progressive_details.night_shift_charge`) — so when EasyBooking's `fare_policy` row gets seeded (section 8, Data config), night charge comes along with it for free. Nothing extra to build.

## Design decisions (confirmed / made explicit for review)

| Aspect | Decision | Rationale |
|---|---|---|
| Destination model | Destination-less, like Rental (`estimatedDistance`/`estimatedDuration` given by rider, no route calc at search) | User confirmed "like rental" |
| Fare policy | `ProgressiveDetails` (same formula OneWay uses) via `fare_product` config pointing at a Progressive `FarePolicy` row | User's core ask |
| End-ride OTP | A `Bool` flag carried **inside the `TripCategory` value itself** (`EasyBooking Bool`), read from a new `TransporterConfig` field at the moment the category is constructed (search time). `isEndOtpRequired (EasyBooking otpRequired) = otpRequired`. | User wants a genuine on/off switch, not a hardcoded value. Embedding the flag in `TripCategory` means it rides through the entire booking lifecycle (Quote → Booking → Ride) for free — `StartRide.hs`/`EndRide.hs` need **zero changes**, since they already call the pure `isEndOtpRequired booking.tripCategory` helper. |
| Pricing policy (Estimate vs Quote) | `QuoteBased True` — same as Rental (fixed price at on_search, no live Select step) | Consistent with "no destination, no live route" — there's nothing to estimate a range against |
| Mode type | **None.** `EasyBooking` takes only the one `Bool` flag — no `OneWayMode`/`TripMode` parameter, no RideOtp/OnDemandStaticOffer split. One category, one flag, one flow. | User corrected this explicitly: not two variants multiplied by the flag — just one true/false switch |
| Odometer / snap-to-road rectify | `isOdometerReadingsRequired = False`, `shouldRectifyDistantPointsSnapToRoadFailure = True` | Mirrors Rental exactly (same GPS-tracked, destination-less shape prone to the same distant-point snap issue) |
| Toll applicability | `isTollApplicableForTrip _ (EasyBooking _) = True` | It's Progressive-priced like OneWay, which already charges tolls |
| Night charge basis | No explicit entry needed — `isFixedNightCharge = isRentalTrip \|\| isInterCityTrip` already evaluates to `False` for EasyBooking, giving it OneWay's dynamic time-window night charge (correct, since it shares OneWay's fare policy) | Falls out automatically, no code needed |
| Go-home / driver-pool-skip | `isGoHomeAvailable = False` (catch-all), `skipDriverPoolCheck = True` (catch-all) | Matches Rental's current behavior, lowest risk |

## Flow-ordered summary (work through in this order)

| # | Ride-flow phase | API | Files that need changes |
|---|---|---|---|
| 1 | Search (foundation) | Rider `POST /rideSearch` → Beckn `search` | `Trip.hs` (new constructor), `RiderPreferredOption.hs` (new constructor) |
| 2 | Search — rider-app outgoing | same | `Beckn/OnDemand/Transformer/Search.hs` (rider-app): `riderPreferredOptionToCategory` |
| 3 | Search — driver-app incoming, fare policy resolved, Quote built | Beckn `search` → `on_search` | `Beckn/OnDemand/Utils/Search.hs` (driver-app): `mapCategoryCodeToRiderPreferred`; `Merchant.yaml` (new `TransporterConfig` field); `Domain/Action/Beckn/Search.hs`: `getPossibleTripOption` |
| 4 | Rider app receives quotes | Beckn `on_search` | `Domain/Action/Beckn/OnSearch.hs` (rider-app): preference-based quote filter **+** the wire-level `QuoteDetails` type & `buildQuote` parser in the same file (section 7a, Layer C) — a new `EasyBookingDetails`/`EasyBookingQuoteDetails` shape |
| 4a | (rider-app only) Quote/Booking domain shapes | n/a — this is the domain-model layer underneath phases 4-5, not a separate API call | `spec/Storage/Quote.yaml`, `spec/Storage/Booking.yaml` (new `EasyBookingDetails` enum entries), `SharedLogic/Confirm.hs`'s `buildBookingDetails` (section 7a, Layers A & B) |
| 5 | Confirm → driver accept → start → end | `POST /confirm`, Beckn `init/confirm`, `POST /start`, `POST /end` | **No changes** beyond 4a above — `StartRide.hs`/`EndRide.hs`/driver-app `Init.hs`/`Confirm.hs`/`Driver.hs` all dispatch purely off `booking.tripCategory` via the `Trip.hs` helpers |
| 6 | Config/data | n/a | `fare_policy` + `fare_product` + `transporter_config` seed SQL |
| 7 | Safety net | n/a | final grep pass for any other exhaustive `TripCategory` match before `cabal build all` |

Files 1-2 above are done as notes already in this conversation; file 3 (`Transformer/Search.hs`) and file 4 (`Utils/Search.hs`) are next.

## Files to change

### 1. `Backend/lib/beckn-spec/src/Domain/Types/Trip.hs` — the core enum

**What's there now** (verified by reading the file directly):
```haskell
data TripCategory
  = OneWay OneWayMode
  | Rental RentalMode
  | RideShare RideShareMode
  | InterCity OneWayMode (Maybe Text)   -- already shows the pattern: mode + one extra field
  | CrossCity OneWayMode (Maybe Text)
  | Ambulance OneWayMode
  | Delivery OneWayMode
  deriving stock (Eq, Ord, Generic)
```
`ToJSON`/`FromJSON`/`Show`/`Read`/`ToSchema` are all **hand-written** (not derived) — each one enumerates every constructor explicitly, which is why `InterCity`/`CrossCity` (the only existing constructors with an extra field beyond the mode) are the right template to copy for `EasyBooking`'s extra `Bool`.

**What changes:**
- Add `| EasyBooking Bool` (line ~46) — a single constructor, one field: the end-OTP-mandatory flag. No mode parameter, no `EasyBookingMode` type — one category, one flag, one flow.
- `ToJSON`/`FromJSON` (line 105-155): add a plain clause, same shape as every other single-field constructor (`OneWay`/`Rental`/`Ambulance`/`Delivery`):
  ```haskell
  toJSON (EasyBooking otpRequired) =
    object ["tag" .= ("EasyBooking" :: Text), "contents" .= otpRequired]
  -- FromJSON: "EasyBooking" -> EasyBooking <$> v .: "contents"
  ```
- `Show`/`Read` (line 238-333): copy the plain single-field pattern (same as `show (Rental s) = "Rental_" <> show s`): `show (EasyBooking otpRequired) = "EasyBooking_" <> show otpRequired`, with a matching `Read` clause.
- `generateTripCategoryShowInstances` (line 249-260): add `[show (EasyBooking otp) | otp <- [True, False]]` (just 2 entries, not 2×2).
- `ToSchema TripCategory` (line 157-196): add an `EasyBooking` entry to the `oneOf` list using a `Bool` schema for `contents`, same shape as the `OneWay`/`Rental` entries (single schema ref, no extra named field).
- Add explicit clauses to: `tripCategoryToPricingPolicy` (line 346, → `QuoteBased True`), `isEndOtpRequired` (line 363, → the embedded flag directly — see below), `shouldRectifyDistantPointsSnapToRoadFailure` (line 389, → `True`), `isTollApplicableForTrip` (line 421, → `True`). Leave `isRideOtpTrip`, `isOdometerReadingsRequired`, `isGoHomeAvailable`, `skipDriverPoolCheck`, `isRentalTrip`, `isInterCityTrip`, `isAmbulanceTrip`, `isDeliveryTrip`, `isDynamicOfferTrip` on their existing catch-alls (already resolve correctly per the design table — `isRideOtpTrip` in particular has no reason to be true here since there's no RideOtp-style mode anymore).

**The OTP flag itself** — current code (line 363-367):
```haskell
isEndOtpRequired :: TripCategory -> Bool
isEndOtpRequired (Rental _) = True
isEndOtpRequired (InterCity _ _) = True
isEndOtpRequired (Delivery _) = True
isEndOtpRequired _ = False
```
New clause added: `isEndOtpRequired (EasyBooking otpRequired) = otpRequired`. Nothing else in this function changes.

**Important gap to be aware of, found while re-reading `EndRide.hs` (line 339-343):**
```haskell
when (DTC.isEndOtpRequired booking.tripCategory) $ do
  case driverReq.endRideOtp of
    Just endRideOtp -> unless (Just endRideOtp == rideOld.endOtp) $ throwError IncorrectOTP
    Nothing -> pure ()   -- ← missing OTP is silently accepted, even when isEndOtpRequired = True
```
Today, even for Rental (`isEndOtpRequired = True`), the backend does **not** reject an end-ride that omits the OTP — it only validates it *if the driver's app happens to send one*. So "mandatory" is currently enforced by the driver app's UI, not the API. If the flag should make the backend itself refuse to end the ride without an OTP when `otpRequired = True`, that's one extra line in `EndRide.hs`: change the `Nothing -> pure ()` branch to `Nothing -> throwError OtpRequired` (or similar), gated on `isEndOtpRequired`. This would be a **stricter behavior than Rental has today**, and would apply only to `EasyBooking`'s check (a targeted `case booking.tripCategory of EasyBooking _ True -> ...; _ -> {- existing lenient behavior -}` rather than changing the shared branch for all categories). Flagging this explicitly since it changes what "mandatory" actually means at the API level — confirm during review whether you want the stricter reject-if-missing behavior or just today's Rental-equivalent lenient check.

### 2. `Backend/lib/beckn-spec/src/Domain/Types/RiderPreferredOption.hs`
- Add `| EasyBooking` to the enum (line 27-35). This type is fully derived (`Generic`, TH macros) — no hand-written instances to touch.

### 3. `Backend/app/rider-platform/rider-app/Main/src/Beckn/OnDemand/Transformer/Search.hs`
- `riderPreferredOptionToCategory` (line 110-119) — add `DRPO.EasyBooking -> "ON_DEMAND_EASY_BOOKING"` (new Beckn category code). This function has no wildcard, so the compiler forces this addition (`-Werror` safety net working as intended).

### 4. `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Beckn/OnDemand/Utils/Search.hs`
- `mapCategoryCodeToRiderPreferred` (line 270-276) — add `"ON_DEMAND_EASY_BOOKING" -> DRPO.EasyBooking` above the existing catch-all.

### 5. New config field — `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/Merchant.yaml` (`TransporterConfig` block, fields section starts ~line 1061)

**What's there now** (verified): this block already has many `Maybe Bool` toggles, e.g.:
```yaml
    isDynamicPricingQARCalEnabled: Maybe Bool
    recomputeCongestionChargeOnEndRide: Maybe Bool
    enableMobilityBilling: Maybe Bool
```
**What changes:** add one more, following the exact same idiom:
```yaml
    easyBookingEndOtpMandatory: Maybe Bool
```
`Maybe` so existing rows default to `Nothing` (treated as `False`/optional) with no backfill required. After the YAML edit, run `, run-generator` (regenerates the Beam/domain type) — it will also emit the `ALTER TABLE transporter_config ADD COLUMN` migration automatically; no hand-written SQL needed for this column.

### 6. `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Search.hs` — `getPossibleTripOption` (line 982-1023)

**What's there now** (verified):
```haskell
localBundleForPreference = case dsReq.riderPreferredOption of
  DRPO.OneWay -> if isScheduled then [...] else [...]
  DRPO.Rental -> [Rental OnDemandStaticOffer] <> [Rental RideOtp | not isScheduled]
  DRPO.Ambulance -> [Ambulance OneWayOnDemandDynamicOffer | not isScheduled]
  DRPO.Delivery -> [Delivery OneWayOnDemandDynamicOffer | not isScheduled]
  _ -> []
tripCategories =
  if checkIfMeterRideSearch dsReq.isMeterRideSearch
    then [OneWay MeterRide]
    else case dsReq.dropLocation of
      Just _ -> {- InterCity/CrossCity/localBundleForPreference branches -}
      Nothing -> [Rental OnDemandStaticOffer] <> [Rental RideOtp | not isScheduled]  -- ← hardcoded to Rental
```
This function already receives `tConf :: DTMT.TransporterConfig` as a parameter — the new flag from step 5 is already in scope here with no extra threading needed.

**What changes**, two edits:
- `localBundleForPreference` — add: `DRPO.EasyBooking -> [EasyBooking (fromMaybe False tConf.easyBookingEndOtpMandatory)]` — a single-element list, one category, the config flag baked straight in. This is where the flag actually gets set, once, at search time.
- **The `Nothing` branch is a real bug EasyBooking exposes**: it's currently hardcoded to `Rental` regardless of `dsReq.riderPreferredOption`, for *any* destination-less search. Since EasyBooking is also destination-less, an EasyBooking search would silently fall into this branch and resolve to `Rental` categories instead of `EasyBooking` ones unless fixed. Change it to reuse `localBundleForPreference` instead of hardcoding Rental.

### 7. Rider-app quote/estimate filtering — `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/OnSearch.hs`

**Correction after reading the actual code directly** (an earlier note in this plan, based on agent research rather than a direct read, got this wrong — this section replaces it):

Current code, verified (line 404-425):
```haskell
filterQuotesByPrefference' _quotesInfo blackListedVehicles =
  case searchRequest.riderPreferredOption of
    DRPO.Rental -> filter (\qInfo -> not (qInfo.vehicleVariant `elem` ambulanceVariants) && (not $ isNotRental qInfo)) _quotesInfo
    DRPO.OneWay -> filter (\quote -> isNotRental quote && isNotBlackListed blackListedVehicles quote.vehicleCategory && not (quote.vehicleVariant `elem` ambulanceVariants)) _quotesInfo
    DRPO.Ambulance -> filter (\qInfo -> qInfo.vehicleVariant `elem` ambulanceVariants) _quotesInfo
    DRPO.Delivery -> []
    _ -> filter isNotRental _quotesInfo   -- ← catch-all

filterEstimtesByPrefference' _estimateInfo blackListedVehicles =
  case searchRequest.riderPreferredOption of
    DRPO.OneWay -> filter (...) _estimateInfo
    DRPO.InterCity -> filter (...) _estimateInfo
    DRPO.Ambulance -> filter (\eInfo -> eInfo.vehicleVariant `elem` ambulanceVariants) _estimateInfo
    DRPO.Delivery -> filter isDeliveryEstimate _estimateInfo
    _ -> []   -- ← catch-all
```
where `isNotRental qInfo = case qInfo.quoteDetails of RentalDetails _ -> False; _ -> True`.

**What this actually means for EasyBooking:**
- **Quotes**: `EasyBooking` would fall into the `_ -> filter isNotRental _quotesInfo` catch-all. Since an EasyBooking quote's `quoteDetails` will be tagged `EasyBookingDetails` (section 7a), not `RentalDetails`, `isNotRental` returns `True` for it — **it passes through the catch-all correctly, no code change is strictly required here.** Still worth adding an explicit `DRPO.EasyBooking -> filter isNotRental _quotesInfo` branch for clarity/symmetry with the other categories, but it's a nice-to-have, not a bug fix.
- **Estimates**: `EasyBooking` falls into `_ -> []`. This is **exactly correct** — since EasyBooking is `QuoteBased` (not `EstimateBased`), it should never produce estimates, and `Rental` itself relies on this same catch-all today (it has no explicit `DRPO.Rental` branch in `filterEstimtesByPrefference'` either). No change needed.

### 7a. A second, deeper sum-type mirror I missed on the first pass — `QuoteDetails`/`BookingDetails` (rider-app only)

`TripCategory` is not the only place ride-type gets branched on. The **rider-app** additionally has a parallel family of sum types that describe the *shape of the quote/booking data itself*, and these are matched **exhaustively, with no wildcard** in the places that matter — so `EasyBooking` needs a new constructor threaded through here too, separately from everything in sections 1-7. (Verified directly by reading the files below, not from earlier agent summaries.) This layer does **not** exist on the driver-app side — driver-app's `Domain/Types/Quote.hs`/`Booking` are flat, carrying `tripCategory` + `fareParams` directly, confirmed by grep — so nothing below applies there.

**Layer A — the domain `QuoteDetails`/`BookingDetails` sum types** (NammaDSL-generated from YAML):
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/Quote.yaml:38` — `enum: OneWayDetails OneWayQuoteDetails, AmbulanceDetails DriverOffer, InterCityDetails InterCityDetails, RentalDetails RentalDetails, DriverOfferDetails DriverOffer, OneWaySpecialZoneDetails SpecialZoneQuote, DeliveryDetails DriverOffer, MeterRideDetails MeterRideQuoteDetails`
  → add `EasyBookingDetails RentalDetails` (reuse the same `Domain.Types.RentalDetails.RentalDetails` shape — night-shift info etc. — no new module needed).
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/Booking.yaml:143` — `enum: OneWayDetails OneWayBookingDetails, RentalDetails RentalBookingDetails, ...`
  → add `EasyBookingDetails RentalBookingDetails` (reuse `RentalBookingDetails {stopLocation :: Maybe Location}` — matches the destination-less shape decided earlier).
- Both are YAML spec edits → run `, run-generator` to regenerate `src-read-only/Domain/Types/Quote.hs` / `Booking.hs`.

**Layer B — `SharedLogic/Confirm.hs`, `buildBookingDetails`** (line 493-501, verified — no wildcard):
```haskell
buildBookingDetails = case quote.quoteDetails of
  DQuote.OneWayDetails _ -> DRB.OneWayDetails <$> buildOneWayDetails Nothing
  DQuote.AmbulanceDetails _ -> DRB.AmbulanceDetails <$> buildAmbulanceDetails
  DQuote.DeliveryDetails _ -> DRB.DeliveryDetails <$> buildDeliveryDetails
  DQuote.RentalDetails _ -> pure $ DRB.RentalDetails (DRB.RentalBookingDetails {stopLocation = mbToLoc, ..})
  DQuote.DriverOfferDetails driverOffer -> DRB.DriverOfferDetails <$> (buildOneWayDetails driverOffer.isUpgradedToCab)
  DQuote.OneWaySpecialZoneDetails _ -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails
  DQuote.InterCityDetails _ -> DRB.InterCityDetails <$> buildInterCityDetails
  DQuote.MeterRideDetails _ -> DRB.MeterRideDetails <$> buildMeterRideDetails
```
→ add `DQuote.EasyBookingDetails _ -> pure $ DRB.EasyBookingDetails (DRB.EasyBookingBookingDetails {stopLocation = mbToLoc, ..})`, copying the `RentalDetails` clause exactly.

**Layer C — the wire-level `QuoteDetails` local to `Domain/Action/Beckn/OnSearch.hs`** (line 257-262, verified — a *different, smaller* type than Layer A's domain type, hand-written in this file, used only to parse the incoming Beckn `on_search` catalog for `QuoteBased` categories):
```haskell
data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | InterCityDetails InterCityQuoteDetails
  | RentalDetails RentalQuoteDetails
  | OneWaySpecialZoneDetails OneWaySpecialZoneQuoteDetails
  | MeterRideDetails MeterRideQuoteDetails
```
(Note this local type has only 5 constructors, not 8 — `Ambulance`/`Delivery`/`DriverOffer` domain variants are built via a *different* path, `Domain/Action/Beckn/OnSelect.hs`, because those categories are `EstimateBased` and go through `select`/`on_select` rather than arriving as a ready quote at `on_search`. Since we decided `EasyBooking = QuoteBased True` — same as Rental — this OnSearch.hs file is the correct one to extend, not OnSelect.hs.)
→ add `EasyBookingDetails EasyBookingQuoteDetails` here, and a matching data type. **Open design question, not yet resolved:** `RentalQuoteDetails` (line 292-302) carries package-shaped preview fields (`perHourCharge`, `includedDistancePerHr`, `plannedPerKmRate`, `deadKmFare`, `nightShiftInfo`) that describe Rental's *package* pricing — none of these make sense for EasyBooking's Progressive/per-km pricing. Two options to pick between during implementation:
  - **Minimal** — mirror `MeterRideQuoteDetails`'s shape: just `{ id :: Text }` (or `quoteId`), since the actual fare is already fully computed and shown via `QuoteInfo.estimatedTotalFare`; no separate "package preview" data is needed for a Progressive-priced quote.
  - **Fuller** — a small new `EasyBookingQuoteDetails` with OneWay-like preview fields (e.g. `baseFare`, `perExtraKmRate`, `nightShiftInfo`) if the rider-facing UI wants to show a fare breakdown before confirming.
  - This also requires the matching driver-app-side change: wherever the driver-app populates the outgoing `on_search` catalog item for a Quote-based category (this needs one more targeted read during implementation — likely `Beckn/OnDemand/Transformer/OnSearch.hs` or similar on the driver-app side — not yet traced in this plan).
- Also add a `buildEasyBookingDetails`-equivalent parsing function next to the existing `buildRentalDetails` (line 656) in this same file, called from `buildQuote`'s case block (line 578-588, verified — also no wildcard, same "must add a branch or it won't compile" situation as Layer B).

### 8. Data config — no schema changes, only rows (plus the one new config column from step 5)

**Real, verified example rows** (read directly from `Backend/dev/config-sync/master_to_local_sql/atlas_driver_offer_bpp/`, which is the actual local-dev seed data for this exact table set — used here as the template, not hypothetical):

```sql
-- fare_policy row (verified real shape, trimmed to relevant columns):
-- INSERT INTO "atlas_driver_offer_bpp"."fare_policy" ("id", "fare_policy_type", "merchant_operating_city_id", "merchant_id", ...)
--   VALUES ('<new-uuid>', 'Progressive', '<city-id>', '<merchant-id>', ...);

-- fare_policy_progressive_details row (verified real shape):
-- INSERT INTO "atlas_driver_offer_bpp"."fare_policy_progressive_details"
--   ("fare_policy_id", "base_distance", "base_fare", "dead_km_fare", "waiting_charge", "night_shift_charge", ...)
--   VALUES ('<same-uuid-as-above>', '1500', '30', '10', '{"contents":0.75,"tag":"PerMinuteWaitingCharge"}', '{"contents":1.25,"tag":"ProgressiveNightShiftCharge"}', ...);

-- fare_policy_progressive_details_per_extra_km_rate_section row (the tiered per-km pricing):
-- INSERT INTO "atlas_driver_offer_bpp"."fare_policy_progressive_details_per_extra_km_rate_section"
--   ("id", "fare_policy_id", "start_distance", "per_extra_km_rate", "distance_unit")
--   VALUES ('<new-id>', '<same-uuid>', '0', '30.00', 'Meter');

-- fare_product row — THE KEY CONFIRMATION: trip_category is stored as the Show-instance
-- string ('OneWay_OneWayOnDemandDynamicOffer' in real seed data), not JSON. This directly
-- confirms the Show instance planned in section 1 is exactly what this column needs:
-- INSERT INTO "atlas_driver_offer_bpp"."fare_product"
--   ("area", "enabled", "fare_policy_id", "id", "merchant_id", "merchant_operating_city_id", "time_bounds", "trip_category", "vehicle_variant", "search_source")
--   VALUES ('Default', 't', '<fare_policy uuid from above>', '<new-uuid>', '<merchant-id>', '<city-id>', 'Unbounded', 'EasyBooking_True', 'SEDAN', 'ALL');
--   -- 'EasyBooking_True' or 'EasyBooking_False' depending on which OTP behavior this row is for —
--   -- since the flag is embedded in the TripCategory value itself, you may want BOTH a
--   -- 'EasyBooking_True' and 'EasyBooking_False' fare_product row per city/tier if a single
--   -- deployment needs to run both OTP modes side by side (unlikely given the flag is meant to
--   -- be a per-merchant/city setting, but the two rows would simply point at the same fare_policy_id
--   -- either way, since the flag doesn't affect the fare formula, only the OTP behavior).
```
- Can clone rates from an existing OneWay `fare_policy`/`fare_policy_progressive_details` row for the same city/tier (query it first via `SELECT ... WHERE fare_policy_type = 'Progressive'`), or set fresh rates.
- **`transporter_config.easy_booking_end_otp_mandatory`**: set per merchant/city to `true` or `false` — this is what `getPossibleTripOption` (section 6) reads to decide which `EasyBooking_<bool>` value to construct at search time; it does not need to match the `fare_product` row's tag by itself, since only one of the two will actually be selected per search based on this config value.
- Ship these as a SQL seed migration in `Backend/dev/feature-migrations/00XX-easybooking-fare-product-seed.sql` (next number after the existing `0038-...`), following the exact pattern of the real seed files above.

### 9. Grep safety-net pass — every remaining `case ... tripCategory of` site

**What this means:** `-Werror` turns "incomplete pattern match" warnings into build failures. So after adding the `EasyBooking` constructor (section 1), any `case tripCategory of` written without a final `_ -> ...` wildcard will **fail to compile** — which is exactly what we want: the compiler finds every remaining spot for us, instead of us having to manually verify each one is correct by inspection. This section is the full real list (grepped directly, not from memory) so nothing is a surprise when it happens.

**Full list, grepped directly** (`grep -rn "case.*[Tt]ripCategory.*of" Backend/...`):

Rider-app (`Backend/app/rider-platform/rider-app/Main/src/`):
`Tools/Notifications.hs:1707`, `Storage/Queries/Transformers/Booking.hs:156,158,196,198`, `Storage/Queries/Transformers/Quote.hs:38,40`, `Beckn/OnDemand/Transformer/OnSearch.hs:100,159`, `Domain/Action/Internal/ViolationDetection.hs:54`, `Domain/Action/Dashboard/Ride.hs:167`, `Domain/Action/Beckn/OnSearch.hs:433`, `Domain/Action/Beckn/Common.hs:1522,1676`, `SharedLogic/Confirm.hs:259` (**spot-checked directly — has a wildcard, safe, no change needed**), `SharedLogic/MessageBuilder.hs:215`.

Driver-app (`Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/`):
`Storage/Queries/Transformers/Ride.hs:95`, `Storage/Queries/Transformers/Booking.hs:21,30,68`, `Lib/DriverCoins/Coins.hs:300,308,603`, `Beckn/OnDemand/Transformer/Init.hs:40,55`, `Beckn/OnDemand/Utils/OnSearch.hs:188`, `Domain/Action/UI/Driver.hs:1895`, `Domain/Action/UI/Ride/Common.hs:443`, `Domain/Action/Internal/StopDetection.hs:68`, `Domain/Action/Internal/BulkLocUpdate.hs:104`, `Domain/Action/Dashboard/Ride.hs:543`, `Domain/Action/Dashboard/RideFlowDebug.hs:49,55,58`, `Domain/Action/Beckn/Init.hs:254`, `Domain/Action/Beckn/Confirm.hs:296,302`, `Domain/Action/Beckn/Update.hs:239`, `Domain/Action/Beckn/Search.hs:455`, `SharedLogic/FarePolicy.hs:211,259`, `SharedLogic/Cancel.hs:121`, `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers.hs:138`, `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/SendSearchRequestToDrivers.hs:300`, `SharedLogic/Allocator/Jobs/ScheduledRides/ScheduledRideNotificationsToDriver.hs:94`.

Shared lib (`Backend/lib/beckn-spec/src/`):
`BecknV2/OnDemand/Utils/Common.hs:224`, `Domain/Types/FareProductType.hs:36,47` (has catch-alls, low risk — confirmed earlier), `Domain/Types/Trip.hs:390,397,402,410` (these are the `Trip.hs` helper functions themselves, already covered explicitly in section 1).

**How to actually work through this at implementation time**: don't pre-audit all ~30 sites by hand now — add the constructor (section 1), run `cabal build all`, and fix each compile error as it surfaces. That's both faster and safer than manual inspection, since the compiler can't miss one. Sites with an existing wildcard (like `Confirm.hs:259`, spot-checked above) won't even appear as errors — they're already correct by construction (EasyBooking falls through their `_` branch, generally to "not applicable," which is the right default for most of these).

## Explicitly out of scope for this change
- No new fare-calculation formula — reuses `processFPProgressiveDetails` untouched.
- No per-ride override of the OTP flag (e.g. a request-time parameter) — it's set once per merchant/city via `transporter_config`, not decided per individual booking. Flag if you need it decidable per-ride rather than per-city.
- No mobile/frontend UI changes (backend-only; assumed to be driven by an automation/dashboard channel, not the consumer app, based on earlier discussion in this session).

## Verification plan
1. `cabal build all` from `Backend/` after each file group above — expect `-Werror` to flag any missed exhaustive pattern match; fix forward rather than adding wildcards that could silently mis-route other categories.
2. Add a new Postman collection under `Backend/dev/integration-tests/collections/` modeled directly on `RentalRideFlow/01-RentalRideFlow.json` (same call sequence: search → get quotes → confirm → driver accept → start → end → verify), but asserting `tripCategory` contains `"EasyBooking"`. Run it twice against two seeded configs: once with `easy_booking_end_otp_mandatory = false` (end-ride succeeds with no OTP in the body, like Rental's start-OTP-only today) and once with `true` (end-ride without an `endRideOtp` should behave exactly like Rental does today — see the two-OTP note from earlier in this conversation: `EndRide.hs` currently only validates the OTP *if supplied*, so "mandatory" for EasyBooking should be double-checked against whether it needs to additionally reject a missing OTP, which would be a small behavioral change to `EndRide.hs` beyond just the `isEndOtpRequired` flag).
3. Manually confirm via the fare_parameters row on a test ride that the final fare used `FParamsProgressiveDetails` (extraKmFare-based), not `FParamsRentalDetails`.

## Open item to flag back during your review
The Beckn category code `"ON_DEMAND_EASY_BOOKING"` (step 3/4) is a name I chose for this plan — rename freely if you have a different convention in mind; it's a single string literal in two places.
