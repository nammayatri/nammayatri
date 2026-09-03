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
| End-ride OTP | **DECIDED (final): Option A — hardcoded, exactly like `Rental`/`InterCity`/`Delivery` today.** `EasyBooking` is a bare, argument-less constructor (no `Bool`, no config field, no `TransporterConfig` change at all). `isEndOtpRequired (EasyBooking) = <True or False — pick one, see open item below>`. To change later: edit this one line in `Trip.hs`, rebuild, redeploy — same as changing any other hardcoded trip-category behavior. Applies identically to every city/merchant; cannot vary per city without a code change. | User explicitly chose the simpler, non-configurable option after walking through the tradeoff (config flag = changeable without redeploy but per-city, vs hardcode = redeploy needed but zero extra plumbing). Confirmed: no risk in either value — both paths already run in production today for other categories (`True` path via Rental/InterCity/Delivery, `False` path via OneWay/Ambulance/RideShare/CrossCity), so `StartRide.hs`/`EndRide.hs` need **zero changes** either way. |
| Pricing policy (Estimate vs Quote) | `QuoteBased True` — same as Rental (fixed price at on_search, no live Select step) | Consistent with "no destination, no live route" — there's nothing to estimate a range against |
| Mode type | **None.** `EasyBooking` takes **no arguments at all** — not a `Bool`, not a mode. Just a bare constructor, same shape as if `Rental` had no `RentalMode` parameter. | Simplified twice during design: first removed the mode-type split, then (this session) removed the `Bool` flag too in favor of a hardcoded value |
| Odometer / snap-to-road rectify | `isOdometerReadingsRequired = False`, `shouldRectifyDistantPointsSnapToRoadFailure = True` | Mirrors Rental exactly (same GPS-tracked, destination-less shape prone to the same distant-point snap issue) |
| Toll applicability | `isTollApplicableForTrip _ (EasyBooking _) = True` | It's Progressive-priced like OneWay, which already charges tolls |
| Night charge basis | No explicit entry needed — `isFixedNightCharge = isRentalTrip \|\| isInterCityTrip` already evaluates to `False` for EasyBooking, giving it OneWay's dynamic time-window night charge (correct, since it shares OneWay's fare policy) | Falls out automatically, no code needed |
| Go-home / driver-pool-skip | `isGoHomeAvailable = False` (catch-all), `skipDriverPoolCheck = True` (catch-all) | Matches Rental's current behavior, lowest risk |

## Flow-ordered summary (work through in this order)

| # | Ride-flow phase | API | Files that need changes |
|---|---|---|---|
| 0 | **The entry point** — public request schema | Rider `POST /rideSearch` (the request body itself) | `SharedLogic/Search.hs` (new `EasyBookingSearchReq` type + `SearchReq` constructor + `fareProductConstructorModifier` tag), `Domain/Action/UI/Search.hs` (`extractSearchDetails` clause + new `processEasyBookingSearch` function) — **without this, nothing below is reachable** |
| 1 | Search (foundation) | same | `Trip.hs` (new constructor), `RiderPreferredOption.hs` (new constructor) |
| 2 | Search — rider-app outgoing | same | `Beckn/OnDemand/Transformer/Search.hs` (rider-app): `riderPreferredOptionToCategory` |
| 3 | Search — driver-app incoming, fare policy resolved, Quote built | Beckn `search` → `on_search` | `Beckn/OnDemand/Utils/Search.hs` (driver-app): `mapCategoryCodeToRiderPreferred`; `Domain/Action/Beckn/Search.hs`: `getPossibleTripOption` (no `Merchant.yaml`/`TransporterConfig` change needed — end-OTP is hardcoded in `Trip.hs`, not config-driven; see Option A decision above) |
| 4 | Rider app receives quotes | Beckn `on_search` | `Domain/Action/Beckn/OnSearch.hs` (rider-app): preference-based quote filter **+** the wire-level `QuoteDetails` type & `buildQuote` parser in the same file (section 7a, Layer C) — a new `EasyBookingDetails`/`EasyBookingQuoteDetails` shape |
| 4a | (rider-app only) Quote/Booking domain shapes | n/a — this is the domain-model layer underneath phases 4-5, not a separate API call | `spec/Storage/Quote.yaml`, `spec/Storage/Booking.yaml` (new `EasyBookingDetails` enum entries), `SharedLogic/Confirm.hs`'s `buildBookingDetails` (section 7a, Layers A & B) |
| 5 | Confirm → driver accept → start → end | `POST /confirm`, Beckn `init/confirm`, `POST /start`, `POST /end` | **No changes** beyond 4a above — `StartRide.hs`/`EndRide.hs`/driver-app `Init.hs`/`Confirm.hs`/`Driver.hs` all dispatch purely off `booking.tripCategory` via the `Trip.hs` helpers |
| 6 | Config/data | n/a | `fare_policy` + `fare_policy_progressive_details` + `fare_product` seed SQL (no `transporter_config` needed — Option A, hardcoded OTP) |
| 7 | Safety net | n/a | final grep pass for any other exhaustive `TripCategory` match before `cabal build all` |

Section 0 (the `SearchReq` entry point), and files 1-2, are done as notes already in this conversation; file 3 (`Transformer/Search.hs`) and file 4 (`Utils/Search.hs`) are next.

## Files to change

### 0. THE ENTRY POINT — `SearchReq`, the actual public API request-body schema (this is what you see in Swagger)

**This was missing from earlier drafts of this doc — found and designed later in the same session, and it's the most important piece: without it, nobody can even send an EasyBooking search request. Everything in sections 1 onward is unreachable without this.**

`Backend/app/rider-platform/rider-app/Main/src/SharedLogic/Search.hs:61` — the real, verified sum type behind the `oneOf` you see in the `/v2/rideSearch` Swagger doc (`OneWaySearchReq`, `RentalSearchReq`, etc. are its alternatives):
```haskell
data SearchReq = OneWaySearch OneWaySearchReq | RentalSearch RentalSearchReq | InterCitySearch InterCitySearchReq | AmbulanceSearch OneWaySearchReq | DeliverySearch OneWaySearchReq | PTSearch PublicTransportSearchReq | FixedRouteSearch FixedRouteSearchReq
  deriving (Generic, Show)

instance ToJSON SearchReq where toJSON = genericToJSON fareProductOptions
instance FromJSON SearchReq where parseJSON = genericParseJSON fareProductOptions

fareProductConstructorModifier :: String -> String
fareProductConstructorModifier = \case
  "OneWaySearch" -> "ONE_WAY"
  "RentalSearch" -> "RENTAL"
  "InterCitySearch" -> "INTER_CITY"
  "AmbulanceSearch" -> "AMBULANCE"
  "DeliverySearch" -> "DELIVERY"
  "FixedRouteSearch" -> "FIXED_ROUTE"
  x -> x
```
This `fareProductConstructorModifier` is where the Swagger `fareProductType` enum string (`"RENTAL"`, `"ONE_WAY"`, etc.) actually comes from — it's a plain constructor-name-to-string mapping, with a catch-all (`x -> x`) that would otherwise silently pass through the raw Haskell constructor name if unmapped.

**Changes needed:**
1. Add a new request type, `RentalSearchReq`'s minimal sibling (`SharedLogic/Search.hs`, next to `RentalSearchReq` at line 143):
```haskell
data EasyBookingSearchReq = EasyBookingSearchReq
  { origin :: SearchReqLocation,
    startTime :: UTCTime,
    isSourceManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    quotesUnifiedFlow :: Maybe Bool,
    isReallocationEnabled :: Maybe Bool,
    numberOfLuggages :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
```
Deliberately smaller than `RentalSearchReq` — no `stops`, no `estimatedRentalDistance`, no `estimatedRentalDuration` (confirmed: destination-less **and** estimate-less; the quote will show `base_fare` + `per_extra_km_rate` instead of any pre-computed number, per the reviewer's design).

2. Add the constructor to `SearchReq` (line 61): `| EasyBookingSearch EasyBookingSearchReq`

3. Add the tag mapping to `fareProductConstructorModifier`: `"EasyBookingSearch" -> "EASY_BOOKING"` (name open to change, flagged in "Open items")

4. `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs`, function `extractSearchDetails` (line 143-187, verified directly) — add a new clause mirroring the `RentalSearch` one:
```haskell
EasyBookingSearch EasyBookingSearchReq {..} ->
  SearchDetails
    { riderPreferredOption = DRPO.EasyBooking,
      roundTrip = False,
      stops = [],
      hasStops = Nothing,
      returnTime = Nothing,
      driverIdentifier_ = Nothing,
      routeCode = Nothing,
      destinationStopCode = Nothing,
      originStopCode = Nothing,
      vehicleCategory = Nothing,
      currentLocation = Nothing,
      busLocationData = [],
      fromSpecialLocationId = Nothing,
      toSpecialLocationId = Nothing,
      city = Nothing,
      ..    -- RecordWildCards fills origin, startTime, quotesUnifiedFlow, isReallocationEnabled,
            -- numberOfLuggages, isSourceManuallyMoved, isSpecialLocation from EasyBookingSearchReq
    }
```

5. Same file, function `getRouteDetails` (line 585-614, verified) — add a dispatch line and a new processing function:
```haskell
EasyBookingSearch easyBookingReq -> processEasyBookingSearch person easyBookingReq originCity
...
processEasyBookingSearch :: SearchRequestFlow m r => DPerson.Person -> EasyBookingSearchReq -> Context.City -> m (RouteDetails, Maybe Text)
processEasyBookingSearch _person _easyBookingReq _originCity =
  return
    ( RouteDetails
        { longestRouteDistance = Nothing, shortestRouteDistance = Nothing,
          shortestRouteDuration = Nothing, shortestRouteStaticDuration = Nothing,
          shortestRouteInfo = Nothing, multipleRoutes = Nothing
        },
      Nothing
    )
```
Simpler than `processRentalSearch` — no OSRM call, no serviceability check on a stop, since there's no stop concept at all for EasyBooking. Just "where" (origin) and "when" (startTime), nothing else.

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

**What changes (FINAL — Option A, bare constructor, no `Bool`, decided this session):**
- Add `| EasyBooking` (line ~46) — a bare constructor, **no arguments at all**. Not even a mode. Simplest possible addition to this type.
- `ToJSON`/`FromJSON` (line 105-155): a nullary constructor needs its own small shape (no existing constructor in this type has zero fields, so this is a genuinely new — but trivial — case, not a copy-paste of an existing one):
  ```haskell
  toJSON EasyBooking = object ["tag" .= ("EasyBooking" :: Text)]
  -- FromJSON: "EasyBooking" -> pure EasyBooking
  ```
- `Show`/`Read` (line 238-333): `show EasyBooking = "EasyBooking"` — no suffix needed at all (compare to `show (Rental s) = "Rental_" <> show s`, which needs the suffix because `Rental` carries a mode value to encode; `EasyBooking` carries nothing, so the bare tag name is the whole string). Matching trivial `Read` clause.
- `generateTripCategoryShowInstances` (line 249-260): add just `["EasyBooking"]` — one entry, not a list comprehension over modes/flags.
- `ToSchema TripCategory` (line 157-196): add an `EasyBooking` entry to the `oneOf` list with no `contents` schema at all (just the `tag` field, matching the JSON shape above — check whether the shared `tripCategorySchema` helper needs a small tweak to support a schema entry with no `contents`, since every existing entry currently assumes one).
- Add explicit clauses to: `tripCategoryToPricingPolicy` (line 346, → `QuoteBased True`), `isEndOtpRequired` (line 363, → **hardcoded `True` or `False` — see open item below, not yet picked**), `shouldRectifyDistantPointsSnapToRoadFailure` (line 389, → `True`), `isTollApplicableForTrip` (line 421, → `True`). Leave `isRideOtpTrip`, `isOdometerReadingsRequired`, `isGoHomeAvailable`, `skipDriverPoolCheck`, `isRentalTrip`, `isInterCityTrip`, `isAmbulanceTrip`, `isDeliveryTrip`, `isDynamicOfferTrip` on their existing catch-alls (already resolve correctly per the design table).

**The OTP line itself** — current code (line 363-367):
```haskell
isEndOtpRequired :: TripCategory -> Bool
isEndOtpRequired (Rental _) = True
isEndOtpRequired (InterCity _ _) = True
isEndOtpRequired (Delivery _) = True
isEndOtpRequired _ = False
```
New clause: `isEndOtpRequired (EasyBooking) = True` **or** `isEndOtpRequired (EasyBooking) = False` — **literal value still to be picked, see "Open items" at the end of this doc.** Whichever is chosen, it's a one-word change to flip later (edit + rebuild + redeploy), confirmed safe either way since both the `True` path (Rental/InterCity/Delivery) and the `False` path (OneWay/Ambulance/RideShare/CrossCity) are already exercised in production today — `StartRide.hs`/`EndRide.hs` need no changes regardless of which value is picked, since both just call this function and react generically to whatever it returns.

**Note on the `EndRide.hs` leniency quirk** (still true regardless of Option A vs B): today, even when `isEndOtpRequired` is `True` for a category, the backend doesn't reject a missing OTP — it only validates one *if supplied* (`Nothing -> pure ()` at `EndRide.hs:339-343`). This is unrelated to the True/False decision above; it's a separate, pre-existing leniency that applies identically to every category including Rental today. Not something this change needs to touch unless you specifically want stricter enforcement — flagged for awareness, not as required work.

### 2. `Backend/lib/beckn-spec/src/Domain/Types/RiderPreferredOption.hs`
- Add `| EasyBooking` to the enum (line 27-35). This type is fully derived (`Generic`, TH macros) — no hand-written instances to touch.

### 3. `Backend/app/rider-platform/rider-app/Main/src/Beckn/OnDemand/Transformer/Search.hs`
- `riderPreferredOptionToCategory` (line 110-119) — add `DRPO.EasyBooking -> "ON_DEMAND_EASY_BOOKING"` (new Beckn category code). This function has no wildcard, so the compiler forces this addition (`-Werror` safety net working as intended).

### 4. `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Beckn/OnDemand/Utils/Search.hs`
- `mapCategoryCodeToRiderPreferred` (line 270-276) — add `"ON_DEMAND_EASY_BOOKING" -> DRPO.EasyBooking` above the existing catch-all.

### 5. ~~New config field~~ — **not needed (Option A decided)**

An earlier draft of this plan added a new `TransporterConfig` field (`easyBookingEndOtpMandatory: Maybe Bool`) in `Merchant.yaml` to back a per-city configurable switch. **This is no longer part of the plan** — since the end-OTP requirement is now a hardcoded value directly in `Trip.hs` (Option A, decided this session), there is no new config column, no YAML edit, and no migration needed for this. Removed to avoid confusion with anyone reading an older copy of this doc.

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

**What changes**, two edits (simpler now than the earlier config-reading version):
- `localBundleForPreference` — add: `DRPO.EasyBooking -> [EasyBooking]` — that's the entire line. No config lookup, no `tConf` field access, just the bare category value (its `isEndOtpRequired` answer is already fixed in `Trip.hs`, nothing to inject here).
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
-- confirms the Show instance planned in section 1 is exactly what this column needs.
-- With the FINAL bare-constructor design (Option A, no Bool), this is now simply:
-- INSERT INTO "atlas_driver_offer_bpp"."fare_product"
--   ("area", "enabled", "fare_policy_id", "id", "merchant_id", "merchant_operating_city_id", "time_bounds", "trip_category", "vehicle_variant", "search_source")
--   VALUES ('Default', 't', '<fare_policy uuid from above>', '<new-uuid>', '<merchant-id>', '<city-id>', 'Unbounded', 'EasyBooking', 'SEDAN', 'ALL');
--   -- just the literal string 'EasyBooking' now — no _True/_False suffix, since the OTP
--   -- behavior is no longer carried inside the TripCategory value (it's a hardcoded line
--   -- in Trip.hs's isEndOtpRequired instead). One row per city/vehicle-tier, same as any
--   -- other category — no special-casing needed for two variants.
```
- Can clone rates from an existing OneWay `fare_policy`/`fare_policy_progressive_details` row for the same city/tier (query it first via `SELECT ... WHERE fare_policy_type = 'Progressive'`), or set fresh rates.
- No `transporter_config` row/column needed at all (removed along with section 5 above — Option A decision).
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
- No configurable/per-city OTP toggle — Option A was chosen explicitly: a hardcoded value in `Trip.hs`, same as Rental/OneWay today. To change it later means editing that one line and redeploying, not a runtime setting.
- No mobile/frontend UI changes (backend-only; assumed to be driven by an automation/dashboard channel, not the consumer app, based on earlier discussion in this session).

## Verification plan
1. `cabal build all` from `Backend/` after each file group above — expect `-Werror` to flag any missed exhaustive pattern match; fix forward rather than adding wildcards that could silently mis-route other categories.
2. Add a new Postman collection under `Backend/dev/integration-tests/collections/` modeled directly on `RentalRideFlow/01-RentalRideFlow.json` (same call sequence: search → get quotes → confirm → driver accept → start → end → verify), but asserting `tripCategory` is `"EasyBooking"` and that end-ride behaves per whichever `True`/`False` value was chosen for `isEndOtpRequired`.
3. Manually confirm via the fare_parameters row on a test ride that the final fare used `FParamsProgressiveDetails` (extraKmFare-based), not `FParamsRentalDetails`.

## Quick reference — every step with its API call, and exactly where OTP and fare policy are handled

This section exists to answer two questions at a glance: *"which API call am I looking at"* and *"where does OTP / fare policy actually happen"* — without needing to reconstruct it from the file-by-file sections above.

**Step 0 — Rider searches: `POST /rideSearch`**
Files: `SharedLogic/Search.hs`, `Domain/Action/UI/Search.hs`. New `EasyBookingSearchReq` (origin + startTime only), new `extractSearchDetails` clause, new `processEasyBookingSearch` (no route calc). *No OTP/fare policy yet — this just creates the request.*

**Step 1 — `Trip.hs`** *(no direct API call — the core type every later step reads from)*
Add `| EasyBooking`. **🔑 OTP is decided entirely here**: `isEndOtpRequired (EasyBooking) = True/False` — one line, the whole system's OTP behavior. Fare policy: only the *pricing model* (`QuoteBased True`) is set here, not the ₹ numbers.

**Step 2 — `RiderPreferredOption.hs`** — add `| EasyBooking`. Just a label, no OTP/fare policy relevance.

**Step 3 — Rider-app sends Beckn `search`** *(server-to-server, automatic)*
`Beckn/OnDemand/Transformer/Search.hs` — add `"ON_DEMAND_EASY_BOOKING"` tag. No OTP/fare policy — just translating a label for the wire.

**Step 4 — Driver-app receives `search`** *(Beckn `search → on_search`, automatic)*
`Beckn/OnDemand/Utils/Search.hs` (read the tag back), `Domain/Action/Beckn/Search.hs`'s `getPossibleTripOption` (add `DRPO.EasyBooking -> [EasyBooking]`). **🔑 Fare policy is looked up here**: the *unchanged* `SharedLogic/FarePolicy.hs` queries `fare_product WHERE trip_category = 'EasyBooking'`, finds the seeded row (Step 8), hands the resolved Progressive policy to `SharedLogic/FareCalculator.hs`, which computes `base_fare` as the quoted price. No code changes in either fare file — same functions every other category already uses.

**Step 5 — Rider-app receives `on_search`: `GET /rideSearch/{searchId}/results`**
`Domain/Action/Beckn/OnSearch.hs` — new `EasyBookingQuoteDetails` type + parser. This is where the already-computed `base_fare`/`per_extra_km_rate` (from Step 4) get shown to the rider. No new math here, just display.

**Step 6 — Rider confirms: `POST /rideSearch/quotes/{quoteId}/confirm`**
`Quote.yaml`/`Booking.yaml` (new enum entries), `SharedLogic/Confirm.hs`'s `buildBookingDetails` (new case branch, `stopLocation = mbToLoc`). Just saves the booking — no OTP/fare math.

**Step 7 — Beckn `init`/`confirm` → driver accepts → `POST /driver/ride/{rideId}/start` → `POST /driver/ride/{rideId}/end`**
Files: `Init.hs`, `Confirm.hs`, `Driver.hs`, `StartRide.hs`, `EndRide.hs` (all driver-app). **Change: none in any of them.** **🔑 OTP is enforced here**: `StartRide.hs` reads `isEndOtpRequired booking.tripCategory` (Step 1's answer) and generates/skips the OTP accordingly — zero new code. **🔑 Fare policy is recalculated for real here**: `EndRide.hs` re-resolves the policy and reruns `calculateFareParameters` on the *actual* GPS distance — same unchanged Progressive formula, producing the final charged fare.

**Step 8 — Data/config (SQL, not code)**: `Backend/dev/feature-migrations/00XX-easybooking-fare-product-seed.sql` — `fare_policy` (type Progressive), `fare_policy_progressive_details` (real `base_fare`/`base_distance`/etc.), the per-km rate section, and the `fare_product` row tagging it all to `'EasyBooking'`. **This is where the actual ₹ numbers live** — nowhere else.

**Step 9 — Safety net**: `cabal build all`, `-Werror` catches anything missed.

**One-line summary**: OTP = decided in Step 1, exercised in Step 7. Fare policy = configured in Step 8, looked up in Step 4, finalized in Step 7. Both reuse existing, unchanged mechanisms — nothing new is being built for either, only new *data* pointing at them.

## Open items to flag back during your review
1. **`isEndOtpRequired (EasyBooking) = ?`** — the literal `True`/`False` value still needs to be picked (Option A mechanism is decided; the actual value isn't yet). Both are equally safe to implement (see the design table above); this is a pure product decision, not a technical one.
2. The Beckn category code `"ON_DEMAND_EASY_BOOKING"` (step 3/4) is a name I chose for this plan — rename freely if you have a different convention in mind; it's a single string literal in two places.
3. `EasyBookingQuoteDetails` preview-field shape (section 7, Layer C) — confirmed "fuller" (baseFare + baseDistance + perExtraKmRate), exact field list still to be finalized against what the rider-facing UI actually wants to display.
