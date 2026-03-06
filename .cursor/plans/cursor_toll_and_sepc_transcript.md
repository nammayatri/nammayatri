# TOLL AND SEPC
_Exported on 06/03/2026 at 14:50:38 GMT+5:30 from Cursor (2.6.12)_

---

**User**

Can you explain me how tollCharges are calculated?

---

**Cursor**

Reading the toll-detection and fare-integration logic to trace the full flow.


Checking where toll prices are defined and how they're stored:


Checking how toll charges flow into the quote/estimate and whether toll VAT is applied:


Checking how toll charges are included in the base fare calculation:


Here’s how **toll charges** are calculated and used in Namma Yatri.

---

## How toll charges are calculated

Toll charges are **not** computed from distance or time. They come from **route-based toll detection** plus **per-toll prices** stored in the DB, and are then fed into fare calculation (and optionally toll VAT).

### 1. **Source of toll amounts: `Toll` table**

Each toll is a row in the **`toll`** table (see `spec/Storage/Toll.yaml` and `Domain.Types.Toll`):

- **`price`** – fixed amount (and currency) for that toll, e.g. ₹50.
- **`tollStartGates`** / **`tollEndGates`** – line segments defining entry and exit of the toll.
- **`name`**, **`isAutoRickshawAllowed`**, **`isTwoWheelerAllowed`**, **`merchantOperatingCityId`**, etc.

So the **toll charge for one toll** is simply **`toll.price.amount`** (configured per toll, not derived by the app).

---

### 2. **Route-based aggregation: `TollsDetector`**

The logic that decides **which** tolls apply and **sums** their prices is in **`SharedLogic/TollsDetector.hs`**.

**Entry point:** `getTollInfoOnRoute merchantOperatingCityId mbDriverId route`

1. **Load tolls** for the operating city:  
   `findAllTollsByMerchantOperatingCity merchantOperatingCityId`.
2. **Filter by route**  
   Keep only tolls whose **start gate** segments lie inside the route’s bounding box.
3. **Walk the route segment by segment**  
   For each segment `(p1, p2)`:
   - Find tolls whose **start gate** (line segment) **intersects** `(p1, p2)`.
   - If for one of those tolls an **exit gate** is found later on the route, the route is considered to have **passed through** that toll.
   - Then:
     - Add **`toll.price.amount`** to the running total.
     - Append toll name and id.
     - Continue with the **remaining route** after that exit.
   - If only entry is seen (e.g. on ride, exit in a later batch), that toll is cached as “pending” for that driver (Redis, 6h) and not charged until the exit is seen (or validated at end ride).

So:

- **Total toll charges** = sum of **`toll.price.amount`** over every toll that the route is detected to pass through (entry + exit both resolved).
- No per-km or per-minute formula; it’s **fixed price per toll × number of tolls on route**.

Relevant function: **`getAggregatedTollChargesAndNamesOnRoute`** (and helpers like `getExitTollAndRemainingRoute`). Complexity is O(route points × toll gate segments).

---

### 3. **Where this is used (estimate / quote / booking)**

- **Fare calculator (UI)**  
  In **`Domain.Action.UI.FareCalculator`** (`calculateFareUtil`), the route comes from `mbRoute` (e.g. from maps). It calls:
  - `TD.getTollInfoOnRoute merchanOperatingCityId Nothing (maybe [] (\x -> x.points) mbRoute)`
  and gets `(tollCharges, tollNames, tollIds, ...)`.  
  So **estimate toll** = same aggregation on the **estimated route**.

- **Search / quote / estimate (Beckn)**  
  In **`Domain.Action.Beckn.Search`**, `buildQuote` and `buildEstimate` receive **`tollCharges`** (and `tollNames` / `tollIds`) that were already computed from the search route (via the same TollsDetector flow elsewhere). Those values are passed into **`CalculateFareParametersParams`** as **`tollCharges`**.

So in all these flows, **“toll charges”** are the **aggregated sum of `toll.price.amount`** for tolls detected on the (estimated or actual) route.

---

### 4. **Inclusion in fare (FareCalculator)**

In **`SharedLogic/FareCalculator.hs`**:

- **`calculateFareParameters`** sets:
  - `tollCharges = addMaybes fp.tollCharges (if isTollApplicableForTrip ... then params.tollCharges else Nothing)`  
  So it uses either:
  - **Fare-policy default** `fp.tollCharges` (optional fixed value from dashboard), or  
  - **Route-based** `params.tollCharges` (from TollsDetector), when toll is applicable for that trip/vehicle.

- **`pureFareSum`** includes toll in the total:
  - `... + (fromMaybe 0.0 fareParams.customerCancellationDues + fromMaybe 0.0 fareParams.tollCharges + fromMaybe 0.0 fareParams.parkingCharge) + ...`

So **toll charges** are added as a **single line item** to the fare (and to breakups as “TOLL_CHARGES”), not recalculated inside the fare formula.

---

### 5. **Optional: Toll VAT (FareCalculatorV2)**

If **FareCalculatorV2** is enabled and the fare policy has **`toll_tax_charge_config`** (e.g. `{"value":"25%","appliesOn":["TollChargesComponent"]}`):

- A **toll VAT** is computed as that percentage (or fixed value) **on the toll charges** (the `TollChargesComponent`).
- This is stored as **`tollVat`** and also included in **`pureFareSum`**.

So:

- **Toll charges** = route-based sum of `toll.price.amount`.
- **Toll VAT** (if configured) = configured % (or fixed) **on** that sum.

---

### 6. **End ride and “pending” tolls**

If the driver’s route had only **entry** to a toll (exit not seen in any batch), at **end ride** the system can still charge that toll via **`checkAndValidatePendingTolls`** in `TollsDetector.hs`: it matches pending tolls to **estimated toll IDs** and, if they match, adds those tolls’ **`price.amount`** so the final fare includes them.

---

## Summary

| Aspect | How it works |
|--------|----------------|
| **Per-toll amount** | Stored in DB: `Toll.price.amount` (fixed per toll). |
| **Which tolls apply** | Route segments are checked against `tollStartGates` / `tollEndGates`; if both entry and exit are detected on the route, that toll is counted. |
| **Total toll charges** | Sum of `toll.price.amount` over all tolls the route passes through (from `TollsDetector.getTollInfoOnRoute` / `getAggregatedTollChargesAndNamesOnRoute`). |
| **In fare** | That sum is passed as `tollCharges` into fare params and added in `pureFareSum`; optionally a **toll VAT** is applied on it via FareCalculatorV2. |
| **Fallback** | If route-based tolls aren’t available, fare policy’s fixed `fp.tollCharges` can be used (e.g. in `SearchTry.buildTripQuoteDetail`). |

So **tollCharges** are “calculated” by **detecting which tolls the route crosses** and **summing their configured prices**, not by a distance/time formula.

---

**User**

Keep only tolls whose start gate segments lie inside the route’s bounding box.


explain this, what is bounding box, which tables are used to check this

---

**Cursor**

Here’s a concise breakdown of the bounding box and which data is used.

---

## 1. What is the “bounding box”?

The **bounding box** is the **smallest axis-aligned rectangle** (in latitude/longitude) that **contains every point of the route**.

Conceptually it has four values:

- **minLat** – minimum latitude among all route points  
- **maxLat** – maximum latitude  
- **minLon** – minimum longitude  
- **maxLon** – maximum longitude  

So it’s a rectangular “envelope” around the full route. The same idea appears in the React Native code:

```645:654:ny-react-native/consumer/src-v2/multimodal/utils/locationUtils.ts
export const getBoundingBox = (path: LatLongType[]) => {
    const lats = path.map(p => p.lat);
    const lons = path.map(p => p.lon);
    return {
        minLat: Math.min(...lats),
        maxLat: Math.max(...lats),
        minLon: Math.min(...lons),
        maxLon: Math.max(...lons),
    };
};
```

In the backend, this is implemented in the shared kernel as:

- **`getBoundingBox :: RoutePoints -> BoundingBox`**  
  Builds the `BoundingBox` that encloses all `LatLong` points in the route.

So: **bounding box = rectangle (minLat, maxLat, minLon, maxLon) derived only from the list of route points.** It is **not** read from any table.

---

## 2. Where does the “route” come from?

The **route** is **not** from a database table. It is an **in-memory** value:

- **Type:** `RoutePoints` = `[LatLong]` (list of lat/lon waypoints).
- **Source:** Usually from the maps/route API (e.g. OSRM) when you get the path between pickup and drop (e.g. `mbRoute` in the fare calculator, or the driver’s tracked waypoints).

So:

- **Bounding box** = computed from this list of points with `getBoundingBox route`.
- **No table** is used for the route or the bounding box.

---

## 3. Which table is used for the check?

The **only table** involved in “keep only tolls whose start gate segments lie inside the route’s bounding box” is:

**`toll`** (in schema `atlas_driver_offer_bpp`).

Relevant columns:

- **`toll_start_gates`** – array of **line segments** (entry gates of the toll). Stored as `text[]`; in code they are `[LineSegment]`.
- **`toll_end_gates`** – array of line segments (exit gates). Same type.

A **LineSegment** is a line between two points (e.g. two lat/lon pairs), representing a gate segment on the ground.

Tolls are loaded by operating city with:

- **Query:** `findAllTollsByMerchantOperatingCity` (from `Storage.CachedQueries.Toll`).
- **Filter:** `merchantOperatingCityId = <given city>`.

So the **tables used** for this step are only the **`toll`** table (and only to get tolls and their `toll_start_gates` / `toll_end_gates`). The route (and thus the bounding box) is not from a table.

---

## 4. The exact check (which tolls are kept)

From `TollsDetector.hs`:

```184:185:Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/TollsDetector.hs
      let boundingBox = getBoundingBox route
          eligibleTollsThatMaybePresentOnTheRoute = filter (\toll -> any (\lineSegment -> lineSegmentWithinBoundingBox lineSegment boundingBox) toll.tollStartGates) tolls
```

Meaning:

1. **`boundingBox = getBoundingBox route`**  
   Compute the rectangle that encloses all route points (no table).

2. **For each toll** (from the `toll` table), look at **`toll.tollStartGates`** (each entry is a `LineSegment`).

3. **Keep the toll** if **at least one** of its start-gate segments satisfies:
   - **`lineSegmentWithinBoundingBox lineSegment boundingBox`**  
   So: that segment is **entirely inside** the box **or** **intersects** it (per the shared kernel description).

So:

- **“Tolls whose start gate segments lie inside the route’s bounding box”** = tolls for which at least one segment in `toll.tollStartGates` is contained in or intersects the route’s bounding box.
- **Tables used to check this:** only **`toll`** (for `toll_start_gates`). The route and bounding box are in-memory.

---

## 5. Short summary

| What | Source |
|------|--------|
| **Bounding box** | Smallest lat/lon rectangle containing all route points. Computed with `getBoundingBox route`. Not from a table. |
| **Route** | List of `LatLong` waypoints (from maps/route API or driver track). In-memory, not a table. |
| **Toll start/end gates** | From **`toll`** table: columns **`toll_start_gates`**, **`toll_end_gates`** (array of line segments). |
| **Check** | For each toll, keep it if **any** of its **start gate** segments is inside or intersects the route’s bounding box (`lineSegmentWithinBoundingBox`). |

So: **bounding box = rectangle from the route points; the only table used for this filter is `toll` (and only its gate segments).**

---

**User**

If for one of those tolls an exit gate is found later on the route, the route is considered to have passed through that toll.


how is this checked?

---

**Cursor**

Showing how the code checks that an exit gate is found on the route:


Here’s how “exit gate found later on the route” is actually checked in code.

---

## 1. Two parts: entry then exit

- **Entry:** For the current route segment `(p1, p2)`, the code finds tolls whose **start gate** intersects that segment.
- **Exit:** For those same tolls, it then looks along the **same route** to see if **any** segment of the route intersects one of the toll’s **end gates**. That’s what “exit gate found later on the route” means in implementation terms.

---

## 2. Where the exit is checked: `getExitTollAndRemainingRoute`

```122:130:Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/TollsDetector.hs
-- This function is triggered when allTollCombinationsWithStartGates are found intersecting the route to find the exit segment intersection on the further route and return the remaining route after intersection.
getExitTollAndRemainingRoute :: RoutePoints -> [Toll] -> Maybe (RoutePoints, Toll)
getExitTollAndRemainingRoute [] _ = Nothing
getExitTollAndRemainingRoute [_] _ = Nothing
getExitTollAndRemainingRoute _ [] = Nothing
getExitTollAndRemainingRoute (p1 : p2 : ps) tolls = do
  let mbExitToll = find (\Toll {..} -> any (doIntersect (LineSegment p1 p2)) tollEndGates) tolls
  case mbExitToll of
    Just toll -> Just (p2 : ps, toll)
    Nothing -> getExitTollAndRemainingRoute (p2 : ps) tolls
```

Meaning:

- **Input:**  
  - `(p1 : p2 : ps)` = the **full remaining route** (starting from the segment where we just detected an entry).  
  - `tolls` = the list of tolls that had their **start gate** intersecting the segment where we found the entry (so we only look for exits for those tolls).

- **Check for this segment:**  
  For the segment from `p1` to `p2`, it asks: does this segment **intersect** any of the **end gates** of any of those tolls?  
  That is:  
  `mbExitToll = find (\Toll {..} -> any (doIntersect (LineSegment p1 p2)) tollEndGates) tolls`  
  So “exit gate found” = **some** `tollEndGates` line segment intersects the route segment `(p1, p2)`.

- **If an exit is found:**  
  Return `Just (p2 : ps, toll)`. The route is considered to have **passed through** that toll; we add its charge and continue with the **remaining route** `(p2 : ps)`.

- **If no exit on this segment:**  
  Recurse with the route shortened by one point: `getExitTollAndRemainingRoute (p2 : ps) tolls`. So we now check the **next** segment `(p2, p3)` for intersection with any of the tolls’ **end gates**, and so on. So we keep moving “later on the route” until we either find an exit or run out of route.

So “exit gate found later on the route” is implemented by: **walking the route segment by segment** and, for each segment `(p1, p2)`, checking **`doIntersect (LineSegment p1 p2) tollEndGate`** for each end gate of the candidate tolls. The first segment that intersects any toll’s end gate is the exit segment for that toll.

---

## 3. How this is used in the main loop

From the main aggregation function:

```137:152:Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/TollsDetector.hs
getAggregatedTollChargesAndNamesOnRoute mbDriverId route@(p1 : p2 : ps) tolls (tollCharges, tollNames, tollIds, tollIsAutoRickshawAllowed, tollIsTwoWheelerAllowed) = do
  let allTollCombinationsWithStartGates = filter (\Toll {..} -> any (doIntersect (LineSegment p1 p2)) tollStartGates) tolls
  if not $ null allTollCombinationsWithStartGates
    then do
      case getExitTollAndRemainingRoute route allTollCombinationsWithStartGates of
        Just (remainingRoute, toll) ->
          getAggregatedTollChargesAndNamesOnRoute
            mbDriverId
            remainingRoute
            ...
            ( tollCharges + toll.price.amount,
              tollNames <> [toll.name],
              ...
            )
```

Flow:

1. For the **current** segment `(p1, p2)`:
   - Find tolls whose **start gate** intersects `(p1, p2)` → `allTollCombinationsWithStartGates`.
2. With the **whole remaining route** `route = (p1 : p2 : ps)` and those tolls, call **`getExitTollAndRemainingRoute route allTollCombinationsWithStartGates`**.
3. That call:
   - First checks segment `(p1, p2)` for intersection with any of those tolls’ **end gates**.
   - If not found, checks `(p2, p3)`, then `(p3, p4)`, … until it finds a segment that intersects an end gate or the route ends.
4. If it returns **`Just (remainingRoute, toll)`**:  
   - That toll is considered **passed through** (entry was already seen; exit just found on this segment or a later one).  
   - Its charge is added and we continue with `remainingRoute`.
5. If it returns **`Nothing`**:  
   - No exit found on the rest of the route → we don’t charge yet; we treat it as “pending” (e.g. cache for on-ride and possibly resolve at end of ride).

So the **only** geometric check for “exit gate found later on the route” is: **some route segment `(p_i, p_{i+1})` intersects some line segment in `tollEndGates`**, implemented as **`doIntersect (LineSegment p1 p2) tollEndGate`** in `getExitTollAndRemainingRoute`. The “later” part is simply: we scan from the current segment onward until we find such an intersection.

---

**User**

What is fare breakup? Is it a table or is it a api response to rideSearch API?

---

**Cursor**

Fare breakup is used in **two** ways: as **data in API/Beckn responses** and as a **persisted table** on the rider app.

---

## 1. Fare breakup as a **concept**

**Fare breakup** = list of line items that add up to the total fare, e.g.:

- BASE_FARE, SERVICE_CHARGE, DEAD_KILOMETER_FARE, DISTANCE_FARE  
- TOLL_CHARGES, PARKING_CHARGE, WAITING_OR_PICKUP_CHARGES  
- NIGHT_SHIFT_CHARGE, PLATFORM_FEE, RIDE_VAT, TOLL_VAT  
- etc.

Each item has a **description** (label) and an **amount**.

---

## 2. Fare breakup in **API / Beckn responses** (including search)

Yes, breakup is part of the **response** of search-related and quote flows; it is **not only** from the DB.

- **Beckn Search (on_search):**  
  BPP builds breakup from fare parameters (`mkFareParamsBreakups` in FareCalculator) and sends it in **item tags** (e.g. FARE_POLICY tag group) as the rate card / quotation breakup. So the **ride search (on_search) response** does include fare breakup.

- **Quote API:**  
  Quote response includes `quoteFareBreakup` / `quoteBreakupList` (e.g. `QuoteBreakupAPIEntity`: title + price).

- **Beckn Init (on_init):**  
  BPP sends `fareBreakups` to the BAP; the BAP then **persists** them (see below).

So fare breakup **is** part of the **API/Beckn response** for search, quote, and init.

---

## 3. Fare breakup as a **table** (rider app / BAP)

On the **rider platform (rider-app)**, fare breakup is also stored in a **DB table**.

**Table:** `fare_breakup` (from `spec/Storage/FareBreakUp.yaml`), in the rider-app storage layer.

| Column / concept | Type | Meaning |
|------------------|------|--------|
| `id` | Id FareBreakup | Primary key |
| `description` | Text | e.g. "TOLL_CHARGES", "BASE_FARE" |
| `amount` | Price | Amount + currency |
| `entityType` | FareBreakupEntityType | BOOKING, RIDE, INITIAL_BOOKING, BOOKING_UPDATE_REQUEST |
| `entityId` | Text | Booking id or Ride id |

So each row is one breakup line item attached to a **booking** or a **ride**.

- **When created:**  
  When the BAP processes **on_init**, it creates `FareBreakup` rows for the **booking** (`createFareBreakup`). When the ride ends, it creates/updates breakup rows for the **ride** (e.g. in Common.buildFareBreakup / `QFareBreakup.createMany`).

- **When used:**  
  APIs that need “breakup for this booking/ride” (e.g. invoice, journey, support) read from this table via `findAllByEntityIdAndEntityType`.

So: **fare breakup is a table** on the rider app, keyed by `entityId` + `entityType` (booking or ride).

---

## 4. Short summary

| Question | Answer |
|----------|--------|
| Is fare breakup a **table**? | **Yes.** In rider-app: `fare_breakup` table (description, amount, entityType, entityId). |
| Is it part of **API/Beckn response** (e.g. ride search)? | **Yes.** Sent in on_search (item tags), quote API (`quoteFareBreakup`), and on_init (`fareBreakups`). |
| Where is the table? | **Rider platform only** (atlas_app). BPP (dynamic-offer-driver-app) does **not** have this table; it builds breakup from fare params and sends it in responses. |

So: **Fare breakup is both** — a **table** (on rider/BAP side, for booking/ride) and **data in API/Beckn responses** (including the ride search / on_search flow).

---

**User**

So can you explain the whole flow of toll charges in ride search, update, end and what tables refelect this toll charge both on rider and driver side like fare breakup, fare param etc, which apis are involved?

---

**Cursor**

Here’s a concise end-to-end flow of toll charges and where they live on rider and driver side.

---

# Toll charges: flow and storage (search → update → end)

## 1. Ride search (BPP)

**Trigger:** BAP sends **search** (Beckn); BPP handles it and may call maps for route.

**Flow:**

1. **Route**  
   `Domain.Action.Beckn.Search.handler` gets a serviceable route (from BAP or by calling maps). It has `routePoints` (pickup → drop).

2. **Toll on route**  
   `getTollInfoOnRoute merchantOpCityId Nothing serviceableRoute.routePoints` (from `TollsDetector`) returns `(tollCharges, tollNames, tollIds, ...)`.

3. **Search request**  
   `buildSearchRequest` is called with `mbTollCharges`, `mbTollNames`, `mbTollIds` and creates a **search_request** row.

4. **Quotes/estimates**  
   For each quote/estimate, `buildQuote` / `buildEstimate` call `calculateFareParametersV2` with `tollCharges = tollCharges` (from search). Fare params get **tollCharges** and it’s part of **fare breakup** (e.g. TOLL_CHARGES) in the rate card.

5. **Response to BAP**  
   **on_search** sends items with **item tags** (FARE_POLICY): breakup includes TOLL_CHARGES. BAP parses it (e.g. `getTollCharges`, `buildTollChargesInfo`) and gets `tollChargesInfo` for estimates.

**Driver-side tables (search):**

| Table              | Schema                     | Toll-related fields                          |
|--------------------|----------------------------|----------------------------------------------|
| **search_request** | atlas_driver_offer_bpp     | `toll_charges`, (toll_names/ids in flow)     |
| **estimate**       | (BPP builds in memory; not a table on BPP for search) | — |
| **quote**          | (same; quote has fareParams with tollCharges) | —   |

**Rider-side (search):**

| Table       | Schema   | Toll-related fields                                      |
|------------|----------|----------------------------------------------------------|
| **estimate** | atlas_app | `toll_charges`, `toll_names` (or `tollChargesInfo`)      |

**APIs:** Beckn **search** → BPP **on_search**. BAP stores estimates (with `tollChargesInfo`) from on_search.

---

## 2. Select / Init (booking created)

**Trigger:** User selects an estimate; BAP sends **select** then **init**; BPP confirms and creates booking.

**Flow:**

- **Init:** BPP builds booking from search request.  
  `tollCharges = if isTollApplicable then searchRequest.tollCharges else Nothing`  
  So **booking** gets the **estimated** toll from the search request.

**Driver-side tables (init):**

| Table     | Schema                 | Toll-related fields   |
|----------|------------------------|------------------------|
| **booking** | atlas_driver_offer_bpp | `toll_charges` (estimated) |

**Rider-side (init):**

- **on_init** can carry fare breakups; BAP may create **fare_breakup** rows for **booking** (entityType = INITIAL_BOOKING / BOOKING). Toll appears as one of the breakup items (e.g. TOLL_CHARGES) if sent.

**APIs:** Beckn **select**, **init** → BPP **on_select**, **on_init**.

---

## 3. Update (edit destination / trip update)

**Trigger:** User/driver changes destination (or similar trip update). BPP recalculates route and fare.

**Flow:**

1. New route (e.g. new drop) → **getTollInfoOnRoute** with **driver id** and new route points (e.g. `getTollInfoOnRoute merchantOperatingCity.id (Just person.id) shortestRoute.points` in Update.hs).
2. New **fare params** (including `tollCharges = mbTollInfo <&> (\(tollCharges, _, _, _, _) -> tollCharges)`) are computed and a **booking_update_request** (or similar) is created.
3. BPP sends **on_update** (e.g. edit destination) to BAP with new quote/breakup; that breakup can include updated TOLL_CHARGES.

**Driver-side:**  
- **fare_parameters** (new row for the update) has `toll_charges`.  
- **booking_update_request** carries the new estimated fare/breakup (toll is part of it).

**APIs:** Beckn **update** (e.g. edit destination) → BPP **on_update**.

---

## 4. On-ride (location batches → toll detection)

**Trigger:** Driver app sends location batches; **location-updates** service (or BPP) runs snap-to-road and toll detection.

**Flow:**

1. Waypoints (and optionally interpolated points) are passed to **getTollInfoOnRoute merchantOpCityId (Just driverId) routePoints** so entry/exit gates are detected along the **actual** path (and pending tolls are cached per driver).
2. When a batch is processed and toll is computed, **updateTollChargesAndNamesAndIds** is called (from location-updates or BPP): it **updates the ride** row with current cumulative toll.

**Driver-side tables (on-ride):**

| Table | Schema                 | Toll-related fields                          |
|-------|------------------------|----------------------------------------------|
| **ride** | atlas_driver_offer_bpp | `toll_charges`, `toll_names`, `toll_ids`     |

**Redis (driver):**  
- `onRideTollChargesKey`, `onRideTollNamesKey`, `onRideTollIdsKey` accumulate toll per batch.  
- TollsDetector **pending tolls** cache (entry seen, exit not yet): `tollStartGateTrackingKey driverId`.

**APIs:**  
- No direct “toll API” to app; ride is updated in DB; **on_status** (e.g. ride_ended) later sends final breakup including toll.

---

## 5. End ride (BPP)

**Trigger:** Driver ends the ride; BPP runs end-ride logic.

**Flow:**

1. **Final toll**  
   - If distance/snap-to-road succeeded: final toll is the **ride**’s `tollCharges` (from on-ride updates), possibly merged with **validated pending tolls** (entry was seen but exit only at end: `checkAndValidatePendingTolls`).  
   - If distance failed: fallback to estimated toll or validated pending, depending on logic in `EndRide.hs` (e.g. `updRide.estimatedTollCharges`, `mbValidatedPendingToll`).  
   Result is `(tollCharges, tollNames, tollIds, tollConfidence)`.

2. **Fare params**  
   Final fare is recalculated (e.g. `calculateFinalValuesForCorrectDistanceCalculations` or similar). Those **fare_params** include **tollCharges** (from the ride’s final toll above). A **fare_parameters** row is stored and linked to the ride (`fareParametersId`).

3. **Ride row**  
   Ride is updated with `tollCharges = mbUpdatedFareParams >>= (.tollCharges)` (and names/ids/confidence as applicable).

4. **Send to BAP**  
   `sendRideCompletedUpdateToBAP` → **on_status** (ride_completed).  
   Quote is built with **mkRideCompletedQuote ride fareParams** → **mkFareParamsBreakups** → breakup list includes **TOLL_CHARGES**.  
   This breakup is sent in the Beckn order (quotationBreakup).

**Driver-side tables (end ride):**

| Table              | Schema                 | Toll-related fields                          |
|--------------------|------------------------|----------------------------------------------|
| **ride**           | atlas_driver_offer_bpp | `toll_charges`, `toll_names`, `toll_ids`, `toll_confidence`, `estimated_toll_charges`, `estimated_toll_names`, `estimated_toll_ids` |
| **fare_parameters**| atlas_driver_offer_bpp | `toll_charges` (in the final fare breakup)   |

**APIs:** BPP sends Beckn **on_status** (event = ride_completed) to BAP with **order.quote.quotationBreakup** (includes TOLL_CHARGES).

---

## 6. BAP receives ride_completed (rider side)

**Flow:**

1. BAP receives **on_status** (ride_completed), parses **order.quotationBreakup** into **fareBreakups** (list of description + amount), including **TOLL_CHARGES**.
2. **rideCompletedReqHandler** (Common) updates **ride** (e.g. fare, chargeableDistance, **tollConfidence** from tags) and builds **FareBreakup** entities from the breakup list.
3. **QFareBreakup.createMany** inserts **fare_breakup** rows with **entityId = ride.id**, **entityType = RIDE**, **description** = e.g. "TOLL_CHARGES", **amount** = toll amount.

**Rider-side tables (end ride):**

| Table          | Schema   | Toll-related fields / role                               |
|----------------|----------|----------------------------------------------------------|
| **ride**       | atlas_app| `toll_confidence` (from BPP tags; no `toll_charges` column) |
| **fare_breakup** | atlas_app | Rows with **entity_id** = ride id, **entity_type** = RIDE, **description** = "TOLL_CHARGES", **amount** = toll |

**APIs:** BPP **on_status** (ride_completed) → BAP **on_status** handler. Rider app then reads fare breakup (e.g. invoice / ride details) from **fare_breakup** or from APIs that expose it.

---

## 7. Summary: tables and APIs

**Driver (BPP) – tables that “reflect” toll:**

| Table             | Toll-related columns / usage |
|-------------------|------------------------------|
| **search_request**| `toll_charges` (estimated from route at search) |
| **booking**       | `toll_charges` (estimated at init) |
| **ride**          | `toll_charges`, `toll_names`, `toll_ids`, `toll_confidence`, `estimated_toll_charges`, `estimated_toll_names`, `estimated_toll_ids` |
| **fare_parameters** | `toll_charges` (used in breakup for quote/ride completed) |
| **fare_policy**   | `toll_charges` (optional default/fallback) |

**Rider (BAP) – tables that “reflect” toll:**

| Table         | Toll-related columns / usage |
|---------------|------------------------------|
| **estimate**  | `toll_charges`, `toll_names` (or tollChargesInfo) |
| **quote**     | `toll_charges`, `toll_names` (or tollChargesInfo) |
| **ride**      | `toll_confidence` only (no toll amount column) |
| **fare_breakup** | Rows with description = "TOLL_CHARGES" and amount; **entity_type** = BOOKING or RIDE, **entity_id** = booking/ride id |

**APIs involved:**

| Phase   | API / Beckn flow        | Toll in payload |
|---------|-------------------------|-----------------|
| Search  | search → **on_search**  | Item tags (FARE_POLICY) → TOLL_CHARGES; BAP maps to estimate.tollChargesInfo |
| Select  | select → on_select      | Estimate/quote can carry toll info |
| Init    | init → **on_init**      | Booking created with estimated toll; fare breakups can include TOLL_CHARGES |
| Update  | update → **on_update**  | New quote/breakup with updated TOLL_CHARGES for new route |
| On-ride | (internal)              | Ride row updated via **updateTollChargesAndNamesAndIds** (no separate toll API) |
| End ride| **on_status** (ride_completed) | order.quote.quotationBreakup with TOLL_CHARGES → BAP creates **fare_breakup** rows for ride |

So: **toll is stored on driver side in search_request, booking, ride, and fare_parameters; on rider side in estimate, quote, and fare_breakup (and ride only for tollConfidence).** It flows via **search, init, update, and on_status (ride_completed)**.

---

**User**

 
"
Things to always remember: 
1. If changes needs to be done related to DB and it has yaml file, make the changes to yaml file. Then I will run namma-dsl command to autogenerate code corresponding to that.


Currently we have logic to calculate dynamic tolls in TollDetector. Logic to calculate static Toll is in FarePolicy.TollCharges. 

We have to:
1. Remove the logic of static toll calculation completely. Wherever I am talking about tolls below will be about dynamic tolls
2. Add state entry permit charges logic (starting with DB(yaml) changes and then read only changes which I will run through “, run-generator” command and then ). The plan for it is below but it is not detailed as there are a lot of doubts in it.
3. We have to implement it in exactly similar way as tolls as in the DB columns will be in the same table where dynamic toll Charges was stored earlier (except fare policy tollcharges, i think that was static toll charge). Also there will something synonymous to tollNames, tollIds for stateEntryPermit Charges. The file tollDetector is common similarly stateEntryPermitCharges will be something common where logic will be stored.
4. So on backend these will be 2 separate fields toll charges and state entry permit charges but when we send them on frontend in our api we will club them into tollCharges key only. So backend know this as 2 separate charges but frontend only knows one charge which is sum of both.
5. The logic to calculate state entry permit charges in end ride is dependent on Location tracking service, if it fails then state charges needs to be handled in the same way as tolls if possibly with 3 types of certainty like Sure(when we have both entry and exit), neutral, unsure. [This needs to be discussed in detail in planning and any question about it needs to be asked clearly before implementation because I am not fully aware how this will be implemented.]

Create a detailed plan based on this discussion, which includes step wise changes that we have to make in every flow where dynamic  toll charges is part of rideSearch, UpdateRide, EndRide and more if any. 



StateEntryPermitCharges (Model)(Let me know if more things are required here):
id
amount
city
state
merchant_operating_city_id
merchant_id
bounding_box: 4 point polygon (we can either store 4 points in array or a bounding box, TODO: If anything has been stored as such, we store geojson, we can do same here and check if RBB intersects with this bounding box polygon)
geom: foreign Key to Geometry Table




Flow:
Get all the allowed_state of source from merchant_state.allowed_states table
Get all the rows from StateEntryPermitCharges table whose state matches the state fetched in Step 1.
Get all the Bounding Box of rows selected in Step 2 i.e SBB.
Construct Route Polyline's bounding box (A rectangle covering route) i.e RBB
Check if any side  of RBB intersects with any side of SBB or is withing SBB [This is added so we can avoid heavy computation of State Geometry * Route Polyline computation with every state]
Yes: Fetch the Geom corresponding to that state and check if any consecutive pair's in route's polyline  intersect with any consecutive pair of geom fetched. If Yes, add the charges. [How do we check if we exited and entered another state] [If a route is zig zagging between 2 states we calculate all charges]
We iterate each pair, we check if intersection is found with any state, if found this is exiting that state. From that exit point we travel further to calculate an entry point to another State. When entry point is found corresponding to that state, we add the charges corresponding to entry point and not exit point and iterate on points after that. What if the entry point is not found corresponding to the exit point for some reason? What if entry point is found and exit point is not found and then next entry point comes, how do I handle this by actually knowing that the next point is exit point and not entry point. [We can handle it same way as done in EndRide for Tolls, check if we can copy the same logic, if not we have to check what has to be changed.]
	
If not, ignore that state.





We don’t have to write these cases separately in code, is should automatically be handled in how the logic we have written above to filter based on SBB and check intersection with route point of the area geometry.

Case 1:
Intracity
Completely: Source destination and all polyline points are in the same area.
If any  RBB’s any side does not intersect (either RBB is completely inside or completely outside the SBB) with any SBB’s side  of any state, we ignore the calculation.
Partial: Source and destination are in same area but polyline is in different area.
If any RBB’s side intersects with any SBB’s side and ride is intracity then we calculate further using 5.1 in the Flow above


 2. Cross City:
Source and destination are in different area and destination is in allowed states.
If any RBB’s side intersects with any SBB’s side and ride is intracity then we calculate further using 5.1 in the Flow above


 3. Intercity:
Source and destination is in different area and destination is not in allowed states.
If any RBB’s side intersects with any SBB’s side and ride is intracity then we calculate further using 5.1 in the Flow above


Doubts(Engineering):
How to create a bounding box for testing in geojson.io? What is the query to create GEOM corresponding to geojson generated on geojson.io by connecting 4 points?
Any existing code to get 4 sides of bounding box or give line segment corresponding to start and end lat-long?
How will the logic of entry and exit work for state crossing as done in Toll Table?
What will be our column to match Geometry table with State Entry Permit Charge Table and Merchant State table to fetch allowed state?
Do we have existing function for chehcking intersection and when bounding box is within and all that. We can use existing I think like if its within it is either completely inside or outside. [If there no existing function for core operation let me know]



Doubts(Product): 
Do we have to show tollCharges and stateEntryPermitCharges separately to user or should we club them? @Manan.
- For now we will show clubbed on frontend in tollCharges but keep it different in backend at every place

If you have any doubts about anything feel free to ask beforehand.


"







********************************************


Above it the plan I created, just tell me how should i handle "State Entry/Exit Detection Logic: The plan assumes we'll use the same entry/exit detection pattern as tolls (tracking entry points, finding exit points). If location tracking fails, we'll handle it with certainty levels (Sure/Neutral/Unsure) similar to tolls." I was thinking something on the line that when we find a intersection of route polyine we mark it as exit point and corresponding to that exit point I need to find entry point intersection of route with entry of some other area. Once I have this exit-entry pair I will add the state entry permit of entered area.

The problem is that what if the entry correponding to exit is not found  and lets say vehicle enters some other state and exits that as well. Now we have 2 exit points. There can be bunch of edge cases like this, what can be those and can i handle it like done for tolls in end ride. for search and update ride we wont have errors so these cases might not come (right? since they did not come for tolls in search and endride flow).



---

**Cursor**

Checking how tolls handle pending (entry-only) and end-ride validation so we can mirror that for state-entry permits.


Here’s a concise way to handle state entry/exit and the edge cases, aligned with how tolls work.

---

## 1. Charge on “entry” only (no exit–entry pairing)

For **state entry permit**, you only need to charge when the vehicle **enters** a state that has a permit. You don’t need to pair “exit from A” with “entry into B” for the charge itself.

- **Toll:** Charge when we see both **entry** (start gate) and **exit** (end gate) → one charge per pass-through.
- **State permit:** Charge every time we see **entry into** a permit state (route segment crosses boundary into that state). Each such crossing = one charge.

So:

- **Zig-zag (A → B → A → B):** Each “entry into B” is one charge → 2 charges. No need to track exit–entry pairs.
- **Two exits then one entry:** You only care that at some point you see “entry into B”. One entry → one charge. The two exits don’t create extra charges.
- **Entry found, exit not found:** You already charged when you saw the entry. No “pending” needed for “entry but no exit”.

So the core rule: **whenever a route segment (p_i, p_{i+1}) crosses from outside state X into state X (and X has StateEntryPermitCharges), add one charge for X.** Iterate the route and count such crossings. That gives correct behaviour for zig-zag and multiple crossings.

---

## 2. When do we need “pending” (like tolls)?

For **tolls**, “pending” is: we saw **entry** (start gate) but never saw **exit** (end gate) in any batch, so we don’t charge in the batch; we cache “pending” and at end ride we **validate against estimated toll IDs** and only then apply charges for those missing IDs.

For **state permits**, the only case that needs “pending” is the **reverse** situation:

- We see **exit** from some state (we left a state) but in that batch (or before end of ride) we **never see an entry** into the next state.
- So we know “we’re in the middle of a crossing” but we don’t know which state we entered (e.g. next batch never came, or tracking failed).

Then at **end ride** you do the same as tolls:

- You have **estimated state permit charges** (from search/booking): which state permits we expected to hit (e.g. IDs or state codes).
- You have **detected** state permit charges: states we actually charged (because we saw an entry).
- **Missing** = estimated − detected.
- If we have **pending “incomplete crossing”** (we saw an exit but no entry for that crossing) and **missing** is non-empty, apply charges for the missing state permit(s) with **Neutral/Unsure**, same as tolls.

So you **don’t** need to “find the entry corresponding to an exit”. You only need:

1. **Normal case:** Detect every “entry into state X” and charge X each time.
2. **Pending case:** Detect “exit from some state” with no following “entry into any state” in the data we have → mark “pending state permit crossing”. At end ride, resolve using **estimated** state permits (apply missing ones with Neutral/Unsure).

---

## 3. Edge cases and how they map to toll logic

| Case | What happens | How to handle (same idea as tolls) |
|------|----------------|------------------------------------|
| **Exit found, entry not found** | We left a state; we never see entry into the next (batch end / tracking gap). | Cache “pending state permit crossing”. At end ride: if we have **estimated** state permit IDs and **missing** IDs (expected but not detected), and we have pending, apply charges for missing IDs (Neutral/Unsure). Never charge pending without estimate (same as toll). |
| **Two exits, one entry** | Two “exits”, then one “entry into B”. | Charge only on **entry into B** (once). Exits don’t create extra charges. No special case. |
| **Entry found, exit not found** | We saw entry into B; we never see exit from B. | We already charged B when we saw the entry. No pending for “entry but no exit”. |
| **Zig-zag (A→B→A→B)** | Multiple re-entries into B. | Each “entry into B” is one charge. Count crossings-into-B. No pairing. |
| **Location tracking fails** | No or partial route. | Same as tolls: use **estimated** state permit charges from search/booking; use certainty Sure/Neutral/Unsure based on what we have (e.g. full route vs partial vs only estimate). |

So:

- **Search / Update:** Full route is available; you run the same “detect all entries into permit states” on the full route. No batching, no pending, no “exit without entry” issues.
- **On-ride (batched):** You may see “exit” in one batch and “entry” in a later batch. If you see exit but no entry in the data so far, that’s the only “pending” case; at end ride you resolve with estimated state permits.
- **End ride:** Combine (1) all detected state permit charges (from “entry” events) with (2) validated pending (missing estimated IDs when we have pending crossing), and assign Sure/Neutral/Unsure like tolls.

---

## 4. Concretely: what to implement (aligned with tolls)

1. **Detection**
   - For each permit state X with geometry:
     - For each route segment (p_i, p_{i+1}): if segment crosses from **outside X** to **inside X**, count one **entry into X** and add one StateEntryPermitCharge for X.
   - No need to “pair” exit with entry for charging; only need to detect “entry into X”.

2. **Pending (on-ride only)**
   - When you detect “exit from some state” (segment crosses from inside some state to outside) but in the current batch you do **not** see any “entry into any state” after that exit:
     - Cache “pending state permit crossing” (e.g. per driver: flag or count), similar to `tollStartGateTrackingKey` and pending tolls.
   - When a later batch (or end of route) shows an “entry into B”, charge B and you don’t need to treat that crossing as pending anymore.

3. **End ride**
   - **Estimated state permit charges:** From search/booking (same place you store estimated toll IDs), store which state permit IDs (or state codes) we expect.
   - **Detected:** State permit IDs we actually charged (because we saw entry).
   - **Missing** = estimated − detected.
   - **Pending:** Do we have “pending state permit crossing” (exited but no entry seen)?
   - If pending and missing non-empty: apply charges for **missing** state permit IDs (from DB), with Neutral/Unsure.
   - If no estimate for state permits: do **not** charge pending (same as toll: “Pending toll found WITHOUT estimate IDs - don't charge”).

4. **Certainty**
   - **Sure:** We have full route and all state entries we expected were detected (no pending, no missing).
   - **Neutral:** Some detected + some from validated pending (like “some detected + some pending” for tolls).
   - **Unsure:** Only pending applied (no or partial detection), or distance/tracking failed and we fell back to estimate.

So: **you do not need to “find the entry corresponding to an exit”.** You charge on **entry** only; you use **exit** only to know “we’re in the middle of a crossing” and then at end ride you use **estimated** state permits to decide what to charge for that pending crossing, exactly like tolls. That’s how you handle “entry corresponding to exit not found” and “two exits then one entry” and zig-zag without extra pairing logic.

---

**User**


lets says user exits area 1, enters are 2, exits area 2, enters area 3, exits area 3, enters area 2

But we need to know which intersection is exit and which is entry, to determine this we have to check if we have previous exit then next would be entry, if yno previous exit then the intetsection would be exit, correct? 


I am saying when we got an exit from area 1 but did not get entry into area 2 because of some issue, now when we see again an intersection it will be exit from area 2 but since we just had exit from area 1 we will consider this exit from area 2 as entry and the subsequent entry will also be distored as now the pairing came out to be distorted, correct?

---

**Cursor**

### Why “alternate exit/entry” is not safe

Yes—if you *only* label intersections by “if I saw an exit last time, next must be entry” (or vice‑versa), it **will drift** the moment you miss one event.

Your example:

- exit Area1 → enter Area2 → exit Area2 → enter Area3 → exit Area3 → enter Area2

If you **miss** “enter Area2” after “exit Area1”, then the next real crossing you see might be “exit Area2”. With the alternation heuristic, you’d incorrectly label it as an **entry**, and from that point onward **every label can be flipped**. So your concern is correct.

### The robust way: determine entry/exit from geometry, not from sequence

You should not infer entry/exit from “previous event parity”. Instead, for each route segment \((p_i \to p_{i+1})\), decide transitions by **membership**:

- Compute which area contains \(p_i\) (call it `areaBefore`)
- Compute which area contains \(p_{i+1}\) (call it `areaAfter`)
- If `areaBefore != areaAfter`, then on that segment you have:
  - **exit** = `areaBefore` (if not null)
  - **entry** = `areaAfter` (if not null)
  - Charge the **entry** area (if it has permit)

This works even if you “miss” an intersection event, because you’re not relying on toggling; you’re relying on “outside→inside” vs “inside→outside”.

### What if “contains” is ambiguous (points on boundary, GPS noise, tracking gaps)?

That’s where you mirror toll end-ride behavior:

- **Boundary ambiguity**: if point is on/near border, use a small epsilon buffer or treat as `Unknown` and require the *next* stable point to decide.
- **Tracking gaps**: if you can’t confidently compute `areaAfter` for a while (no points / snap failure), do **not** flip entry/exit guesses. Keep state as `Unknown` and later reconcile using:
  - estimated state-permit IDs from search/booking (apply missing ones with Neutral/Unsure), similar to toll pending validation.

### Key takeaway

- **Do not** use “previous was exit so next is entry” as the primary rule.
- Use **inside/outside classification per segment** to label entry/exit.
- If classification fails, keep it **unknown/pending** and reconcile at end ride using **estimate-based validation** (like tolls), rather than letting parity drift.

---

**User**

But dont you think there is chance that the boundaries(geom of 2 areas in geometry table) might have some gap in between due to some human error. Minor oone but enough such that the areaBefore is sourceCity and areaAfter is some unknown area then we find another intersection where areaBefore is unknow area and are after is destination area?

---

**Cursor**

Yes, that can happen: a small gap between two area polygons (digitization error, different sources, etc.) so that for some segment:

- **areaBefore** = source city (known)
- **areaAfter** = **no polygon** (point falls in the gap) → treat as “unknown”

and later:

- **areaBefore** = unknown
- **areaAfter** = destination area (known)

So you get: exit source → “enter unknown” → “exit unknown” → enter destination.

### Why this is still safe

Rule: **charge only when `areaAfter` is a known area that has a state entry permit.**

- “Enter unknown”: you don’t charge (unknown is not a permit area).
- “Exit unknown” → “enter destination”: you charge destination once.

So you still get exactly one charge for the destination area. The gap is just a strip where no polygon contains the point; you never assign a permit to “unknown”.

### Pending / estimate when the gap is involved

- You see: exit source, then “entry unknown” (no charge).
- If the next batch never arrives, you never see “entry destination”. So you have: we exited a known area and the only “entry” we saw was into unknown.

That’s exactly the **pending** case:

- You have “exited something, but no entry into a known permit area yet”.
- At end ride you have **estimated** state permit(s) (e.g. destination). **Detected** = [].
- You apply the estimated permit charge with Neutral/Unsure, same as tolls.

So a gap doesn’t break the logic; it just means “unknown” is never charged, and any missing charge is recovered via estimate-based validation at end ride.

### Optional hardening

If you want to be explicit in code:

- When **areaAfter** is “no polygon” / unknown, still treat it as “we left areaBefore” (for pending: “we have an incomplete crossing”).
- When **areaBefore** is unknown and **areaAfter** is a known permit area, treat it as “entry into that area” and charge once.

So: boundaries with small gaps are fine as long as you **only charge on entry into a known permit area** and use **estimate-based pending** when you only saw exit (or exit → unknown) and never saw entry into a known state.

---

**User**

So you are saying for every consecutive pairs of points on route i check if it intersects with geom of any allowedstates. If it intersects then there can be follwing cases:

1. Intersects with 2 geoms (how will we idetify which is before and which is after in this case?)
2. Intersects with 1 geom (before known and after unknown | before unknown and after known)



---

**Cursor**

### First correction: don’t start with “segment intersects geom”

For **entry/exit** you want: “did we move from outside→inside (entry) or inside→outside (exit) for a state polygon?”

The clean way per consecutive pair \((p_i, p_{i+1})\) is:

1. Compute **`stateBefore = containingState(p_i)`** (Maybe State)
2. Compute **`stateAfter  = containingState(p_{i+1})`** (Maybe State)
3. If `stateBefore == stateAfter` → no transition
4. If different → you have a transition: exit = `stateBefore`, entry = `stateAfter` (charge on **entry** if it has a permit)

Only if either endpoint is **Unknown** (not contained in any state polygon), or you detect overlap/ambiguity, you fall back to boundary-intersection logic.

This avoids a lot of confusion about “intersection with 1 geom vs 2 geoms”.

---

### Case 1: “Intersects with 2 geoms” — what does that really mean?

It can mean **three different things**, and you handle them differently:

#### 1A) **Normal border crossing between two states**
Segment crosses the boundary between State A and State B, so it “touches” both polygons.

- You don’t need to guess “before/after” from intersections.
- You get it from endpoint containment:
  - if `p_i ∈ A` and `p_{i+1} ∈ B` → **exit A, enter B**
  - if reversed → **exit B, enter A**

#### 1B) **Overlapping polygons / bad data**
Both states’ polygons contain the same endpoint (or both endpoints). Then `containingState` returns “multiple”.

- You must define a deterministic rule, e.g.:
  - **prefer non-overlap** by data cleanup (best)
  - otherwise choose by **priority** (configured), or **smallest area**, or “most specific” polygon.
- If you can’t decide, mark as **Ambiguous** and treat as **pending/unsure** (don’t charge from live detection; rely on estimate at end ride).

#### 1C) **One segment crosses multiple boundaries (A→B→C)**
This happens when points are too far apart (sparse polyline), so a single segment can cut through multiple states.

- If you only look at endpoints you’ll see `A → C` and you’ll miss “enter B”.
- Fix is **not** “intersection toggling”; it’s to ensure segments are small:
  - Search/Update routes usually have dense polylines (good).
  - On-ride you may need **interpolation/densification** (you already do snap-to-road/interpolated points for tolls).
- If you still want to support it robustly: compute **all intersection points with boundaries**, sort them along the segment, and walk them in order; but this is heavier.

---

### Case 2: “Intersects with 1 geom” (known/unknown mixes)

This typically arises from **gaps** or **boundary points**.

#### 2A) before known, after unknown
`stateBefore = A`, `stateAfter = Unknown`

Interpretation: you likely **exited A into a gap** (or boundary ambiguity).

- Do **not** invent an “entry state”.
- Record: “we exited A, next entry unknown” and set a **pending crossing flag**.
- Later, when you eventually see `Unknown → B`, you treat that as **entry into B** and charge B.
- If tracking ends before you see `Unknown → B`, you resolve using **estimated** state permits at end ride (Neutral/Unsure), exactly like toll pending validation.

#### 2B) before unknown, after known
`stateBefore = Unknown`, `stateAfter = B`

Interpretation: you **entered B from a gap**.

- This is a clean **entry into B** → charge B (once per entry).
- Clear any “pending crossing” flag because you now have the entry.

#### 2C) unknown → unknown but segment intersects a state boundary
This means your two sample points are both in a gap (or too far from the polygon), but the segment passes through a state.

- This is exactly the “sparse points” problem again.
- You either:
  - densify points, or
  - treat as **uncertain** and rely on estimate-based application at end ride.

---

### Practical guidance for your implementation

- **Primary signal**: endpoint containment (`containingState(p)`).
- **Secondary**: boundary intersection only when endpoint containment is Unknown/Ambiguous.
- **Avoid parity logic** (“last was exit so next must be entry”) — it breaks immediately if you miss one event.
- **Handle multi-crossing per segment** by ensuring **dense enough points** (best), otherwise you need to split by intersection points.

If you tell me what you plan to use for `containingState(p)` (PostGIS `ST_Contains` / `ST_Covers`, or your Geometry table abstraction), I can suggest the exact “boundary-safe” predicate (because `ST_Contains` excludes boundary; `ST_Covers` includes boundary and reduces Unknowns).

---

**User**

For tolls we have an intersect function what do we give it input and what does it give output. Explain using example. If you just find calling in this repo the implementation might be Kernel libary whose code i have shared@shared-kernel 

---

**Cursor**

Here’s how the toll intersection works in this repo and in the shared kernel.

---

## `doIntersect` (Kernel.Utils.ComputeIntersection)

**Signature (from shared kernel):**

```haskell
doIntersect :: LineSegment -> LineSegment -> Bool
```

**Meaning:** It takes two line segments and returns whether they intersect (including touching at an endpoint or along the segment). It uses orientation and “on segment” checks (standard line-segment intersection).

---

## `LineSegment` (shared kernel)

From the memory map:

- **Type:** `data LineSegment` with two fields:
  - `start :: LatLong` – start of the segment  
  - `end   :: LatLong` – end of the segment  

So one segment is defined by two points (e.g. two lat/lon pairs). In code you’ll see it constructed as something like `LineSegment p1 p2` (first point = start, second = end).

---

## How tolls use it in this repo

**1. Entry (start gate):**

```haskell
allTollCombinationsWithStartGates = filter (\Toll {..} -> any (doIntersect (LineSegment p1 p2)) tollStartGates) tolls
```

- **First argument to `doIntersect`:** `LineSegment p1 p2`  
  - One **route segment**: from route point `p1` to next route point `p2` (both `LatLong`).
- **Second argument:** one element of `tollStartGates` (each is a `LineSegment`), e.g. a gate line from the DB.
- **Output:** `Bool` – whether that route segment intersects that gate segment.
- **Usage:** `any (doIntersect ...) tollStartGates` means: “does this route segment intersect **any** of this toll’s start gates?”

**2. Exit (end gate):**

```haskell
mbExitToll = find (\Toll {..} -> any (doIntersect (LineSegment p1 p2)) tollEndGates) tolls
```

- **First argument:** same route segment `LineSegment p1 p2`.
- **Second argument:** one of the toll’s `tollEndGates` (again `LineSegment`).
- **Output:** `Bool` – does this route segment intersect that end gate?
- **Usage:** find a toll for which the **same** segment `(p1, p2)` intersects any of its end gates.

So in both cases:

- **Input 1:** one segment = one consecutive pair of route points `(p1, p2)`.
- **Input 2:** one segment = one gate (start or end) = one `LineSegment` from the toll (two points).
- **Output:** `True` if the two segments intersect, else `False`.

---

## Example

- **Route:** two consecutive points  
  - `p1 = LatLong 12.95 77.60`  
  - `p2 = LatLong 12.96 77.61`  
  So route segment = `LineSegment p1 p2`.

- **Toll “NH-44”:** one start gate stored as a line across the highway, e.g.  
  - gate = `LineSegment (LatLong 12.955 77.605) (LatLong 12.956 77.606)`.

Then:

- `doIntersect (LineSegment p1 p2) gate`  
  - **Input 1:** segment from (12.95, 77.60) to (12.96, 77.61).  
  - **Input 2:** segment from (12.955, 77.605) to (12.956, 77.606).  
  - **Output:** `True` if these two segments intersect (or touch), otherwise `False`.

So in words: **input = (route segment, gate segment), output = whether they intersect.** The repo uses this to decide “did we cross this toll’s start/end gate?” by checking the current route segment against each gate segment.

---

**User**

so lests say the second point of pair is on the line, will it counted as intersection

---

**Cursor**

Yes. If the second point of the pair (p2) lies on the gate line segment, that **does** count as an intersection.

Reason:

- Two line segments intersect if they share at least one point (including an endpoint or a point in the middle of a segment).
- If p2 is on the gate segment, the route segment (p1–p2) and the gate segment share the point p2, so they intersect.
- The shared-kernel module has `onSegment` (“Checks if a point q lies on the line segment formed by p and r”), which is exactly what’s needed to treat “point on segment” as intersection. So the usual implementation of `doIntersect` will return `True` when an endpoint (like p2) lies on the other segment.

So: **second point of the pair on the gate line → intersection → counted.**

---

**User**

Now lets say I point in a pair, is their any exsiting function which can tell me if it lies in which geom (check the geometry table which stores geom)?

---

**Cursor**

Yes. There are existing helpers that use the geometry table to see which geom(s) contain a point.

---

## 1. **Provider platform (dynamic-offer-driver-app)** – `Storage.Queries.Geometry`

**`findGeometriesContainingGps`** – point → all geometries that contain it (no region filter):

```haskell
findGeometriesContainingGps :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LatLong -> m [Geometry]
findGeometriesContainingGps gps = do
  dbConf <- getReplicaBeamConfig
  geoms <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            ( \BeamG.GeometryT {} ->
                containsPoint' (gps.lon, gps.lat)
            )
            $ B.all_ (BeamCommon.geometry BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] geoms)
```

- **Input:** one point `LatLong` (lat/lon).
- **Output:** list of `Geometry` rows from the geometry table whose geom contains that point.
- **Mechanism:** `containsPoint' (gps.lon, gps.lat)` in the DB filter (likely PostGIS `ST_Contains`/`ST_Within` on the stored geom).

**`findGeometriesContaining`** – same idea, but only among given regions:

```haskell
findGeometriesContaining :: LatLong -> [Text] -> m [Geometry]
findGeometriesContaining gps regions = ...
  -- filter: containsPoint' (gps.lon, gps.lat) AND region `in_` regions
```

- **Input:** point + list of region codes (`Text`).
- **Output:** geometries that contain the point and whose `region` is in that list.

So for “which geom does this point lie in?” you can use **`findGeometriesContainingGps`** (or **`findGeometriesContaining`** if you already have a list of allowed regions).

---

## 2. **Rider platform** – `Storage.Queries.Geometry` (rider-app)

Same pattern: **`findGeometriesContaining gps regions`** – point + regions → `[Geometry]` that contain the point and match the region filter, again using `containsPoint' (gps.lon, gps.lat)` on the geometry table.

---

## 3. **Special zone / special location**

- **SpecialZone:** `findSpecialZoneByLatLong point` uses `containsPointGeom (point.lon, point.lat)` on the special_zone geom.
- **SpecialLocation:** `findSpecialLocationByLatLongFull` etc. use `containsPoint (point.lon, point.lat)` on the special location geom.

So “point in which geom?” is already implemented for:

- The **geometry table** (atlas): `findGeometriesContainingGps` / `findGeometriesContaining`.
- Special zone / special location tables: their respective “find by lat/long” functions.

For state-entry-permit logic, if your state boundaries are stored in that same geometry table (or one with a similar geom column), you can reuse **`findGeometriesContainingGps`** (or a variant with a state/region filter) to get “which geom(s) contain this point?” for each endpoint of a segment. **`containsPoint'`** is the low-level predicate that talks to the geometry table; the public API is these query functions.

---

**User**

Get all the allowed_state of source from merchant_state.allowed_states table
Get all the rows from StateEntryPermitCharges table whose state matches the state fetched in Step 1.
Get all the Bounding Box of rows selected in Step 2 i.e SBB.
Construct Route Polyline's bounding box (A rectangle covering route) i.e RBB
Check if any side  of RBB intersects with any side of SBB or is withing SBB [This is added so we can avoid heavy computation of State Geometry * Route Polyline computation with every state]
Yes: Fetch the Geom corresponding to that state and check if any consecutive pair's in route's polyline  intersect with any consecutive pair of geom fetched. If Yes, add the charges.
To check this we check whether the consecutive pair in route intersects the state polyline(This is in geometry table) or not.
 If the pair intersects then there can be these possibilities Check which area the first point lies in and which area the second point lies in (if they are different). 
If the first area is in SEPC Table and Second is Not, we can’t really charge any entry charges because user entered unknown region which is not in SEPC table.
If the first area is in SEPC Table and the region of second area is not unknown and is not same as the first area, we check the second area, we add the charges of this to out stateEntryPermitCharge (do we have to keep account of all the SEPC we added like Toll Names and Toll IDs for debugging, if yest which table). To check in which area the point lies within there are existing functions.
If the route polyline does not intersect we move to next consecutive pair. 




I updated the plan to achieve this stateEntry permit charges, do you think I am missing naything, will this work

---

**Cursor**

Your plan is mostly right; a few clarifications and one logic fix will make it robust.

---

## What works well

- **Bounding-box pre-filter (RBB vs SBB)** to avoid heavy geom work is good.
- **Charge the state we enter** (second area when it’s in SEPC and ≠ first) is the right rule.
- **First in SEPC, second unknown → don’t charge** is correct (and can feed “pending” at end ride).
- **Using existing “which area does this point lie in?”** (`findGeometriesContainingGps` or similar) is the right approach.
- **Keeping SEPC names/IDs** for debugging and for estimated vs detected at end ride is the same pattern as tolls.

---

## Logic fix: when to add SEPC charge

You said:

- “If first area is in SEPC and second is not → don’t charge.”
- “If first area is in SEPC and second is known and not same → add second’s charges.”

You should **also** charge when **first is not in SEPC** (or unknown) and **second is in SEPC**:

- First point: state A (no permit) or unknown  
- Second point: state B (in SEPC)  
→ We **entered B** → add B’s SEPC charge.

So the rule should be:

- **Whenever `areaSecond` is in SEPC and `areaSecond ≠ areaFirst`, add `areaSecond`’s charge.**  
- No need to require “first in SEPC”; only “second in SEPC” and “we crossed” (areaSecond ≠ areaFirst).

You still keep:

- First in SEPC, second unknown → don’t charge (and treat as pending if needed).
- First unknown, second in SEPC → charge second (we entered a permit state).

---

## Where to keep “account” of SEPC (names/IDs)

Same place as tolls, with parallel columns:

- **Ride:** e.g. `state_entry_permit_charges`, `state_entry_permit_names`, `state_entry_permit_ids` (and estimated equivalents if you do end-ride validation).
- **Search request / booking:** same pattern for estimated state entry permit at search/init.
- **Fare params / breakup:** you’ll have a line item for state entry permit (and optionally names/IDs in the response or in a debug structure).  
So: **same tables that today have toll_charges / toll_names / toll_ids** get state-entry-permit equivalents; that’s where you “keep account” for debugging and for comparing estimated vs detected.

---

## Optional simplification: “intersect” vs “point-in-polygon only”

You said: “Check if any consecutive pair intersects the state polyline; if yes, then check which area first/second point lies in.”

You can get the same result **without** an explicit “route segment × state boundary” intersection step:

- For each consecutive pair `(p1, p2)`:
  - `areaFirst  = which state(s) contain p1` (from geometry table, filtered by candidate states after bbox).
  - `areaSecond = which state(s) contain p2`.
  - If `areaSecond` is in SEPC and `areaSecond ≠ areaFirst` → add `areaSecond`’s charge.

That already implies “we crossed into a permit state.” So:

- **Option A (your current plan):**  
  For each candidate state, fetch geom, check “does any route segment intersect this state’s boundary?”; for those segments, then do area-first / area-second and charge when second in SEPC and different.
- **Option B (simpler):**  
  After bbox filter, for each route point get containing state(s) (existing functions); then for each pair, if second in SEPC and second ≠ first, add charge.

Both are correct; B avoids implementing segment–polygon-boundary intersection if you’re happy to do point-in-polygon for all route points for the candidate states.

---

## Edge cases to be explicit about

1. **Multiple geoms containing one point** (overlapping polygons):  
   Define a rule (e.g. prefer one state by priority or smallest area), or treat as ambiguous and don’t charge from that point (or mark uncertain and use estimate at end ride).

2. **Both points “unknown”** but segment actually crosses a state:  
   Either accept that you might miss that crossing (and rely on estimated state permits at end ride), or keep your “intersect boundary then area check” for those segments.

3. **Same state on both sides of segment** (e.g. both on boundary):  
   Your “second ≠ first” rule already avoids charging when both are the same area.

4. **RBB vs SBB:**  
   You need both: “any side of RBB intersects any side of SBB” **or** “RBB fully inside SBB”. You have `doIntersect` for segment–segment and `pointWithinBoundingBox` (so “all four corners of RBB inside SBB” = RBB within SBB). That’s enough.

---

## Summary

- **Fix:** Charge whenever **second** area is in SEPC and **second ≠ first** (don’t require first to be in SEPC).
- **Account of SEPC:** Same tables as tolls (ride, search_request, booking, etc.) with state_entry_permit_charges / names / ids (and estimated versions where needed).
- **Implementation:** Your “intersect then area check” is correct; an alternative is “point-in-polygon only” for both endpoints and then the same charge rule.
- **Edge cases:** Decide how to handle multiple containing geoms and “both unknown” segments; the rest your plan already covers.

---

**User**

The problem is that I cant't query on my DB for finding within for every pair of point it will be too expensive, what do di do

---

**Cursor**

You’re right that one DB “contains/within” query per route point is too expensive. The fix is to **do geometry work in memory** after a **small, fixed number** of DB calls.

---

## 1. Fetch candidate geometries once (few DB calls)

After your RBB vs SBB filter you have a **small set of candidate states** (e.g. 2–5).

- **Option A:** One query that returns the **geom** (or boundary) for each of those states, e.g.  
  `WHERE state IN (...allowed states...) AND id IN (...SEPC geom_ids...)`  
  so you get back the actual polygon/boundary for each candidate.
- **Option B:** One query per candidate state (still only K queries, K small).

So: **no query per route point**; only 1–K queries for the candidate state geometries.

---

## 2. Do “which area” and “did we cross?” in memory

Once you have the polygon (or boundary) for each candidate state in memory:

- **Point-in-polygon:** For each route point, test against each in-memory polygon (or use a small in-memory spatial index so you only test polygons whose bbox contains the point). Use a **library** (e.g. `geos`, `geo`, or similar in Haskell) so you’re not calling the DB.
- **Segment–boundary intersection (if you use that):** You already have segment–segment intersection (`doIntersect`) in the kernel. So you need the **boundary of each polygon as a list of segments** in memory; then “does route segment (p1,p2) intersect this state’s boundary?” is just a loop over segments, no DB.

So the cost is:

- **DB:** 1–K reads to load candidate state geometries.
- **CPU:** O(route points × candidate states) point-in-polygon in memory, and optionally O(route segments × boundary segments) for intersection checks.

---

## 3. If you can’t do point-in-polygon in the app (no library)

Then use **one bulk DB query** instead of one per point:

- Send the **list of route points** (or a subset, e.g. every 2nd point if route is very dense) to the DB.
- Use PostGIS to answer in one go: “for each of these points, which geometry (from this set of geom IDs) contains it?”  
  (e.g. `UNNEST` points and join with `ST_Contains(geom, point)` or use a lateral join.)
- Application gets back something like `[(point_index, geometry_id)]` and then you only do “first vs second area” logic in code, with **no per-point DB round-trips**.

So: either **bulk DB query (many points, one query)** or **fetch geoms once + in-memory point-in-polygon**; avoid “one query per point”.

---

## 4. Practical recommendation

- **Preferred:**  
  - After bbox filter, **fetch candidate state geometries (or boundaries) once** (1–K queries).  
  - In the app, for each route point compute “which state(s) contain this point?” using **in-memory** point-in-polygon (add a small geo lib if needed).  
  - Then for each consecutive pair, if `areaSecond` is in SEPC and `areaSecond ≠ areaFirst`, add the charge.  
  That keeps DB cost **independent of route length**.

- **Alternative:**  
  - Same bbox filter, then **one bulk query** that takes all route points (or a downsampled list) and returns which candidate geom contains each point.  
  - Same “areaFirst / areaSecond” logic in app.  
  Again, **no per-point queries**.

So: **don’t query “within” for every pair; either fetch geometries once and do containment in memory, or do one bulk “which geom contains each point?” query.** That’s how you avoid the expense.

---

**User**



Things to always remember: 
1. If changes needs to be done related to DB and it has yaml file, make the changes to yaml file. Then I will run namma-dsl command to autogenerate code corresponding to that.


Currently we have logic to calculate dynamic tolls in TollDetector. Logic to calculate static Toll is in FarePolicy.TollCharges. 

We have to:
1. Remove the logic of static toll calculation completely. Wherever I am talking about tolls below will be about dynamic tolls
2. Add state entry permit charges logic (starting with DB(yaml) changes and then read only changes which I will run through “, run-generator” command and then ). The plan for it is below but it is not detailed as there are a lot of doubts in it.
3. We have to implement it in exactly similar way as tolls as in the DB columns will be in the same table where dynamic toll Charges was stored earlier (except fare policy tollcharges, i think that was static toll charge). Also there will something synonymous to tollNames, tollIds for stateEntryPermit Charges. The file tollDetector is common similarly stateEntryPermitCharges will be something common where logic will be stored.
4. So on backend these will be 2 separate fields toll charges and state entry permit charges but when we send them on frontend in our api we will club them into tollCharges key only. So backend know this as 2 separate charges but frontend only knows one charge which is sum of both.
5. The logic to calculate state entry permit charges in end ride is dependent on Location tracking service, if it fails then state charges needs to be handled in the same way as tolls if possibly with 3 types of certainty like Sure(when we have both entry and exit), neutral, unsure. [This needs to be discussed in detail in planning and any question about it needs to be asked clearly before implementation because I am not fully aware how this will be implemented.]

Create a detailed plan based on this discussion, which includes step wise changes that we have to make in every flow where dynamic  toll charges is part of rideSearch, UpdateRide, EndRide and more if any. 



StateEntryPermitCharges (Model)(Let me know if more things are required here):
id
amount
city
state
merchant_operating_city_id
merchant_id
bounding_box: 4 point polygon (we can either store 4 points in array or a bounding box, TODO: If anything has been stored as such, we store geojson, we can do same here and check if RBB intersects with this bounding box polygon)
geom: foreign Key to Geometry Table




Flow:
Get all the allowed_state of source from merchant_state.allowed_states table
Get all the rows from StateEntryPermitCharges table whose state matches the state fetched in Step 1.
Get all the Bounding Box of rows selected in Step 2 i.e SBB.
Construct Route Polyline's bounding box (A rectangle covering route) i.e RBB
Check if any side  of RBB intersects with any side of SBB or is withing SBB [This is added so we can avoid heavy computation of State Geometry * Route Polyline computation with every state]
Yes: Fetch the Geom corresponding to that state and check if any consecutive pair's in route's polyline  intersect with any consecutive pair of geoms fetched. If Yes, add the charges.
To check this we check whether the consecutive pair in route intersects the state polyline(This is in the geometry table) or not.
 If the pair intersects then there can be these possibilities: Check which area the first point lies in and which area the second point lies in (if they are different). 
IWhenever areaSecond is in SEPC and areaSecond ≠ areaFirst, add areaSecond’s charge
If the route polyline does not intersect we move to the next consecutive pair. 
[How do we check if we exited and entered another state] [If a route is zig zagging between 2 states we calculate all charges]
We iterate each pair, we check if intersection is found with any state, if found this is exiting that state. 
To do this what we will simply do is we iterate on every pair of route polyline and check if it itersects with any state’s poliline

“From that exit point we travel further to calculate an entry point to another State. When entry point is found corresponding to that state, we add the charges corresponding to entry point and not exit point and iterate on points after that. What if the entry point is not found corresponding to the exit point for some reason? What if entry point is found and exit point is not found and then next entry point comes, how do I handle this by actually knowing that the next point is exit point and not entry point. [We can handle it same way as done in EndRide for Tolls, check if we can copy the same logic, if not we have to check what has to be changed.]”

If not, ignore that state.





We don’t have to write these cases separately in code, is should automatically be handled in how the logic we have written above to filter based on SBB and check intersection with route point of the area geometry.

Case 1:
Intracity
Completely: Source destination and all polyline points are in the same area.
If any  RBB’s any side does not intersect (either RBB is completely inside or completely outside the SBB) with any SBB’s side  of any state, we ignore the calculation.
Partial: Source and destination are in same area but polyline is in different area.
If any RBB’s side intersects with any SBB’s side and ride is intracity then we calculate further using 5.1 in the Flow above


 2. Cross City:
Source and destination are in different area and destination is in allowed states.
If any RBB’s side intersects with any SBB’s side and ride is intracity then we calculate further using 5.1 in the Flow above


 3. Intercity:
Source and destination is in different area and destination is not in allowed states.
If any RBB’s side intersects with any SBB’s side and ride is intracity then we calculate further using 5.1 in the Flow above


Doubts(Engineering):
How to create a bounding box for testing in geojson.io? What is the query to create GEOM corresponding to geojson generated on geojson.io by connecting 4 points?
Should bounding boxe be an array of few points or should I store it as geom in geometry table?
How to generate geom?
Any existing code to get 4 sides of bounding box or give line segment corresponding to start and end lat-long? We can simply store it as array treat consecutive points as lines.
How will the logic of entry and exit work for state crossing as done in Toll Table?
What will be our column to match Geometry table with State Entry Permit Charge Table and Merchant State table to fetch allowed state?
Do we have existing function for checking intersection and when bounding box is within and all that. We can use existing I think like if its within it is either completely inside or outside. [If there no existing function for core operation let me know]
I think we have doIntersect and findGeometriesContainingGps and findGeometriesContaining
What if polyline boundary of 2 states overlap? How will we know which point is exit and which is entry?
When we check a pair we check if second point is within the state if it is inside muleiple state we filter out the state of which start point is part of.



Doubts(Product): 
Do we have to show tollCharges and stateEntryPermitCharges separately to user or should we club them? @Manan.
- For now we will show clubbed on frontend in tollCharges but keep it different in backend at every place

If you have any doubts about anything feel free to ask beforehand.



“Above it the plan I created, just tell me how should i handle "State Entry/Exit Detection Logic: The plan assumes we'll use the same entry/exit detection pattern as tolls (tracking entry points, finding exit points). If location tracking fails, we'll handle it with certainty levels (Sure/Neutral/Unsure) similar to tolls." I was thinking something on the line that when we find a intersection of route polyine we mark it as exit point and corresponding to that exit point I need to find entry point intersection of route with entry of some other area. Once I have this exit-entry pair I will add the state entry permit of entered area.
The problem is that what if the entry correponding to exit is not found and lets say vehicle enters some other state and exits that as well. Now we have 2 exit points. There can be bunch of edge cases like this, what can be those and can i handle it like done for tolls in end ride. for search and update ride we wont have errors so these cases might not come (right? since they did not come for tolls in search and endride flow).”


If a point lies in more than 1 geom, skip it until you find a line which is in single geom to avoid the case where point A is one geom and point B is in multiple Geom




Great, based on our discussion can you update this plan. Make sure don't remove anything but if the doubts have been asnwered or approach has been changed according to our discussion, update this plan.

---

**Cursor**

Here’s an updated plan that keeps your structure, fills in the doubts we’ve resolved, and folds in the approach we agreed on (charge on entry only, no exit–entry pairing, geometry-based detection, and minimizing DB cost).

---

# State Entry Permit Charges – Updated Implementation Plan

## Things to always remember
1. **DB changes:** Make changes in the **YAML** file; run namma-dsl to autogenerate code.
2. **Tolls:** “Tolls” below = **dynamic tolls only**. Remove all **static** toll logic (e.g. FarePolicy.tollCharges).
3. **State entry permit (SEPC):** Implement **in parallel to tolls** – same tables/columns pattern, separate backend fields, clubbed on frontend.
4. **Backend vs frontend:** Backend keeps **tollCharges** and **stateEntryPermitCharges** (and names/IDs) separate; APIs send a single **tollCharges** key = sum of both.
5. **Certainty:** State permit at end ride uses **Sure / Neutral / Unsure** like tolls when location tracking fails or is partial.

---

## 1. StateEntryPermitCharges model (DB / YAML)

**Table: StateEntryPermitCharges (state_entry_permit_charges)**

- `id` (PK)
- `amount` (e.g. HighPrecMoney / Price)
- `city`
- `state`
- `merchant_operating_city_id`
- `merchant_id`
- **bounding_box:** Store so RBB can be checked without loading geom. Options:
  - **Option A:** Array of 4 points (or 5 to close the rectangle), treat consecutive points as sides; or reuse existing BoundingBox type if present.
  - **Option B:** Store as geom in geometry table and reference it (consistent with rest of geo storage).
- **geom_id** (or similar): Foreign key to **Geometry** table (the state boundary polygon used for containment and intersection).

**Resolved (Engineering):**
- Bounding box: Either array of points (consecutive = sides) or a geom; align with how geojson/geom is stored elsewhere (e.g. geometry table). For RBB vs SBB you only need “sides” for intersection / within checks; existing `getBoundingBox`, `lineSegmentWithinBoundingBox`, `doIntersect` can be used.
- Geometry table link: StateEntryPermitCharges has a column (e.g. `geom_id`) referencing Geometry table. Merchant_state / allowed_states link by **state** (and optionally city/region) to filter which SEPC rows (and thus which geoms) are candidates.

---

## 2. Flow (state entry permit calculation)

**Step 1:** Get all **allowed_state** of source from **merchant_state.allowed_states** (or equivalent).

**Step 2:** Get all rows from **StateEntryPermitCharges** whose **state** is in the set from Step 1.

**Step 3:** Get bounding box(es) of rows from Step 2 → **SBB** (one SBB per state, or union – same as your plan).

**Step 4:** Build **RBB** = route polyline bounding box (e.g. `getBoundingBox route`).

**Step 5:** For each candidate state’s SBB: check if **any side of RBB intersects any side of SBB** OR **RBB is fully within SBB** (e.g. all four corners of RBB inside SBB via `pointWithinBoundingBox`). Use existing helpers: `doIntersect` for segment–segment, `pointWithinBoundingBox` for “within”. If no intersection and not within → skip that state.

**Step 6 (candidate states only):** For each state that passed Step 5:

- **Avoid per-point DB cost:** Fetch that state’s **geom (boundary polygon)** once (or fetch all candidate state geoms in 1–K queries). Do **not** call “point within”/“contains” for every route point in the DB.
- **In-memory:** Load polygon(s) into memory. For each **consecutive pair (p1, p2)** of the route polyline:
  - **areaFirst** = which state(s) contain p1 (using in-memory point-in-polygon on fetched geoms).
  - **areaSecond** = which state(s) contain p2 (same, in memory).
  - **Overlap rule:** If **any** of p1 or p2 lies in **more than one** geom (ambiguous), **skip this segment** and move to the next pair until you get a segment where both endpoints resolve to a single geom each (or one unknown). This avoids wrong attribution when boundaries overlap.
  - **Charge rule:** If **areaSecond** is in SEPC and **areaSecond ≠ areaFirst**, add **areaSecond’s** charge (and record name/id for that SEPC for names/IDs list). No need to “pair” exit with entry; charging on **entry** (second point) is enough.
  - **Unknown:** If areaFirst is known and areaSecond is “unknown” (no geom contains p2), do **not** charge; optionally mark “pending crossing” for end ride. If areaFirst is unknown and areaSecond is in SEPC, charge areaSecond (entry into permit state).

**Step 7:** If route segment does not intersect any candidate state boundary (or after applying overlap rule the segment is skipped), move to the next consecutive pair.

**Zig-zag / multiple entries:** By iterating every pair and applying “areaSecond in SEPC and areaSecond ≠ areaFirst”, multiple entries into the same state (e.g. A→B→A→B) are charged each time (same as tolls).

**Entry/exit detection (resolved):**
- Do **not** infer “this is exit / this is entry” by sequence (e.g. “previous was exit so next is entry”). Use **which area contains p1** and **which area contains p2**.
- **Exit** = segment leaves a state (areaFirst = X, areaSecond ≠ X). **Entry** = segment enters a state (areaSecond = Y, areaFirst ≠ Y). We only need to **charge on entry** (areaSecond in SEPC).
- If location tracking fails or we only see “exit” and never “entry into a known state”, handle at **end ride** via **pending + estimated state permit IDs** (same pattern as tolls: apply missing estimated SEPC with Neutral/Unsure).

---

## 3. Cases (Intracity / Cross City / Intercity)

- **Intracity – completely:** Source, destination, and all polyline points in the same area. If **no** RBB side intersects any SBB side and RBB is not within any SBB, skip state permit calculation.
- **Intracity – partial:** Source and destination in same area but polyline goes through another area. If RBB intersects or is within any SBB, run Step 6 (and in-memory pair logic above).
- **Cross city:** Source and destination in different areas, destination in allowed states. If RBB intersects or is within any SBB, run Step 6.
- **Intercity:** Source and destination in different areas, destination not in allowed states. If RBB intersects or is within any SBB, run Step 6.

No separate code paths per case; the same “RBB vs SBB → then in-memory pair + areaFirst/areaSecond” logic covers them.

---

## 4. Where state entry permit is stored (same as tolls)

- **Ride:** e.g. `state_entry_permit_charges`, `state_entry_permit_names`, `state_entry_permit_ids` (and estimated equivalents if used for end-ride validation).
- **Search request / Booking:** Same pattern for **estimated** state entry permit at search/init.
- **Fare params / breakup:** Separate line item for state entry permit; names/IDs can be stored or exposed for debugging.
- **Account of SEPC (like toll names/IDs):** Same tables that have `toll_charges` / `toll_names` / `toll_ids` get the corresponding state_entry_permit columns; frontend still receives one **tollCharges** = toll + state entry permit.

---

## 5. End ride & certainty (like tolls)

- **Sure:** Full route available; all expected state entries detected; no pending.
- **Neutral:** Some detected + some applied from validated pending (estimated state permit IDs that we didn’t see as “entry” but had pending crossing).
- **Unsure:** Only estimated applied (e.g. tracking failed) or partial detection.

**Pending:** When we have “exited a state” (or crossed into unknown) but never saw “entry into a known permit state” in the data, set a pending state-permit crossing. At end ride, compare **estimated state permit IDs** (from search/booking) with **detected** state permit IDs; apply charges for **missing** IDs only if we have a pending crossing (and optionally pickup/drop within threshold, same as tolls). Do **not** charge pending without an estimate.

---

## 6. Engineering doubts – resolved

- **Bounding box for testing (geojson.io):** Draw a rectangle (four points), export as GeoJSON. For DB: store either as array of points (consecutive = sides) or as geom; use same convention as existing geometry storage.
- **Bounding box storage:** Either array of points (e.g. 4 or 5 points) or store as geom in geometry table; ensure RBB vs SBB can use existing `getBoundingBox`, `lineSegmentWithinBoundingBox`, `doIntersect`, `pointWithinBoundingBox`.
- **Four sides of RBB/SBB:** From 4 corners you get 4 segments; consecutive corners form a side. Store SBB the same way (array or geom); no new core function needed.
- **Entry/exit:** Use **areaBefore (p1)** and **areaAfter (p2)** from point-in-polygon; charge when areaSecond is in SEPC and areaSecond ≠ areaFirst. No exit–entry pairing.
- **Geometry table link:** StateEntryPermitCharges has a FK (e.g. `geom_id`) to Geometry table; Merchant_state / allowed_states use **state** (and city/region if needed) to filter SEPC rows and thus which geoms to fetch.
- **Intersection / within:** Use existing `doIntersect`, `lineSegmentWithinBoundingBox`, `getBoundingBox`, `pointWithinBoundingBox`.
- **Point-in-polygon:** Use `findGeometriesContainingGps` / `findGeometriesContaining` only where it’s cheap (e.g. after fetching candidate geoms once). Prefer **fetch geoms once → in-memory point-in-polygon** for every route point to avoid per-point DB cost.
- **Overlapping state boundaries:** If a point lies in **more than one** geom, **skip that segment** until you get a segment where each endpoint is in at most one geom (or one unknown). Then apply “areaSecond in SEPC and areaSecond ≠ areaFirst”.

---

## 7. Product

- **Display:** Frontend shows a single **tollCharges** (sum of dynamic toll + state entry permit). Backend keeps them separate everywhere (DB, APIs, fare breakup, logs).

---

## 8. Summary of approach changes from discussion

- **Charge rule:** Whenever **areaSecond** is in SEPC and **areaSecond ≠ areaFirst**, add **areaSecond’s** charge. No requirement that “first” be in SEPC.
- **No exit–entry pairing:** Use geometry (which area contains p1, which contains p2); do not use “previous was exit so next is entry”.
- **DB cost:** Fetch candidate state geometries once (1–K queries); do point-in-polygon and pair logic **in memory**. If in-memory geom lib is not available, use one **bulk** DB query that returns which geom contains each route point.
- **Gaps:** If areaSecond is “unknown”, don’t charge; use pending + estimated state permits at end ride.
- **Overlap:** If either point is in multiple geoms, skip that segment; proceed when both endpoints resolve to at most one area each (or unknown).
- **Pending & certainty:** Same pattern as tolls: pending when we saw exit (or unknown) but no entry into a known permit state; resolve at end ride using estimated state permit IDs with Sure/Neutral/Unsure.

This is the updated plan with nothing removed from your original structure and all discussed behaviour and doubt resolutions folded in.

---

**User**

In the endride how do we handle the pending tolls, can you explain the full logic. We recalculate the tolls, how doe we do that. Exp,ain detailed logic of it

---

**Cursor**

Here’s the full picture of how pending tolls and “recalculated” tolls work at end ride.

---

# End-ride toll handling: full logic

## 1. What we have at end ride

- **Estimated toll (from search/booking):** `estimatedTollCharges`, `estimatedTollNames`, `estimatedTollIds` (stored on ride/booking at init).
- **Detected toll (during ride):** `ride.tollCharges`, `ride.tollNames`, `ride.tollIds` — updated by the location-tracking flow as batches are processed.
- **Pending tolls (Redis):** key `tollStartGateTrackingKey driverId` — list of **Toll** values where we saw **entry** (start gate) but never saw **exit** (end gate) in any batch.

So at end ride we can have:

- Some tolls **fully detected** (entry + exit) → already in `ride.tollCharges/tollNames/tollIds`.
- Some tolls only **entry seen** → only in Redis as “pending”.
- An **estimate** of which tolls we expected → `estimatedTollIds` (and names/charges).

---

## 2. How tolls get “recalculated” / updated at end ride

Tolls are not recalculated from scratch at end ride. We either **run one last batch** of the same logic used during the ride, or **fall back** to route/estimate.

### 2.1 Where it happens: `finalDistanceCalculation`

On end ride we get **trip end points** from the ride-end API (`tripEndPoints`) and call:

```haskell
finalDistanceCalculation ... rideId driverId tripEndPoints' estimatedDistance 
  estimatedTollCharges estimatedTollNames estimatedTollIds 
  pickupDropOutsideOfThreshold passedThroughDrop isMeterRide
```

That calls **`processWaypoints`** with `ending = True` and `waypoints = tripEndPoints`:

1. **Add** `tripEndPoints` to the driver’s waypoints in Redis.
2. Run **`recalcDistanceBatches`** (with `ending = True`).

So “recalculation” is: process the **same waypoints (and any already stored)** through the same distance + toll pipeline used during the ride.

### 2.2 Two paths inside that pipeline

**Path A – Snap-to-road (and toll) is run**

When `snapToRoadCallCondition` is true (e.g. route deviation, pickup/drop outside threshold, or **estimated toll present**):

1. **`processSnapToRoadCall`** runs:
   - Reads waypoints from Redis (including the trip end points we just added).
   - Processes them in **batches** (e.g. 98 points per batch) via **`recalcDistanceBatchStep`**.
2. For **each batch**, **`interpolatePointsAndCalculateDistanceAndToll`**:
   - Calls **snap-to-road** on the batch → gets `snappedPoints` and distance.
   - If toll is applicable: **`getTollInfoOnRoute merchantOpCityId (Just driverId) snappedPoints`** (TollsDetector).
     - That walks the route segment-by-segment, detects entry/exit per toll, and can:
       - Add to **detected** toll (entry + exit seen) → returns `(tollCharges, tollNames, tollIds)` for **this batch**.
       - Or only see entry → put that toll in **pending** (Redis key `tollStartGateTrackingKey driverId`).
3. When toll is returned for a batch we **accumulate in Redis**:
   - `onRideTollChargesKey`: increment by this batch’s toll amount.
   - `onRideTollNamesKey` / `onRideTollIdsKey`: append names/ids.
4. After each batch (and when not `snapToRoadFailed`), we **read** those Redis keys and call **`updateTollChargesAndNamesAndIds`** → **ride** row is updated with **current total** `tollCharges`, `tollNames`, `tollIds`.

So at end ride, if we run this path, we’re doing **one more pass** over the waypoints (including trip end points): snap-to-road + toll detection, same as on-ride. That can:

- Resolve a **pending** toll (we finally see the exit in this “final” batch).
- Or leave some tolls still only in **pending** (entry seen, exit never seen).

**Path B – Snap-to-road not run**

When `snapToRoadCallCondition` is false:

- We do **not** run `processSnapToRoadCall`.
- We call **`getTravelledDistanceAndTollInfo`**:
  - Tries to get an **undeviated route** from Redis (`multipleRouteKey` from search).
  - If found: **`getTollInfoOnRoute merchantOperatingCityId Nothing routePoints`** on that **search route** (no driver id → no pending).
  - Returns `(distance, tollInfo)`; if no route, uses **estimated** distance and **estimated** toll.
- Then we **`updateDistance`** and, when toll applicable, **`updateTollChargesAndNamesAndIds`** with that toll (route-based or estimated).

So in this path we don’t “re-run” driver waypoints; we use **search route + toll** or **estimate**.

After either path, we **reload the ride** from DB → **`updRide`** now has the latest `tollCharges`, `tollNames`, `tollIds` (either from the last batch update or from the fallback).

---

## 3. How pending tolls are applied: `checkAndValidatePendingTolls`

We only charge **pending** tolls if we have an **estimate** to validate against (and we match by toll **ID**).

**Signature:**

```haskell
checkAndValidatePendingTolls
  driverId
  estimatedTollCharges   -- not used in logic
  estimatedTollNames      -- not used in logic
  estimatedTollIds        -- which tolls we expected
  alreadyDetectedCharges -- current ride.tollCharges
  alreadyDetectedTollIds -- current ride.tollIds
  -> m (Maybe (HighPrecMoney, [Text], [Text]))  -- extra charges, names, ids to add
```

**Steps:**

1. **Read pending tolls** from Redis: `Hedis.safeGet (tollStartGateTrackingKey driverId)` → `Maybe [Toll]` (tolls where we saw entry but not exit).
2. **Branch on (pending, estimated IDs, detected IDs):**

**Case 1: We have pending, estimated IDs, and detected IDs**

- **Missing IDs** = `estimatedTollIds \ alreadyDetectedTollIds` (expected but never fully detected).
- **Matching pending** = pending tolls whose `id` is in **missing IDs**.
- If any:
  - Sum their `price.amount` → **missingCharges**.
  - Collect **missingNames**, **missingIds**.
  - Return **`Just (missingCharges, missingNames, missingIds)`** (to be **added** to detected).
- If none (missing IDs don’t match any pending): return **`Nothing`** (don’t charge pending).
- If **missing IDs** is empty (all estimated tolls were detected): return **`Nothing`**.

**Case 2: We have pending and estimated IDs, but no detected IDs**

- “No tolls detected yet” (e.g. distance/tracking failed and we never wrote any toll to the ride).
- **Matching pending** = pending tolls whose `id` is in **estimatedTollIds**.
- If any: return **`Just (charges, names, ids)`** for those (apply full estimated toll from pending).
- Else: **`Nothing`**.

**Case 3: We have pending but no estimated IDs**

- We do **not** charge pending (safety).
- Return **`Nothing`**.

**Case 4: No pending tolls**

- Return **`Nothing`**.

So: **recalculation** gives us the best **detected** toll we can get from waypoints/route/estimate; **pending** is only applied when we have **estimated toll IDs** and we treat “missing” estimated IDs as justified by the pending cache.

---

## 4. How we merge detected + pending and set confidence (EndRide.hs)

After **`checkAndValidatePendingTolls`** we have:

- **`updRide.tollCharges/tollNames/tollIds`** = what’s in DB (from Path A or B above).
- **`mbValidatedPendingToll`** = `Just (charges, names, ids)` if we’re adding pending, else `Nothing`.
- **`pickupDropOutsideOfThreshold`** → **`canApplyValidatedPendingToll`** = we only apply pending when pickup/drop are **within** threshold (route as expected).

Then we compute **final** `(tollCharges, tollNames, tollIds, tollConfidence)`:

- **Distance calculation failed** (or self-tuned):
  - If we have **estimated** toll and **detected** toll: merge detected + pending (when `canApplyValidatedPendingToll`) → **Neutral**.
  - If we have estimated but **no** detected: use estimated or pending (with Neutral/Unsure) depending on `driverDeviatedToTollRoute` and pending.
  - If we have **no** estimated: we can still use **pending** if we have estimated IDs and `canApplyValidatedPendingToll` → **Unsure**; else no toll or **Nothing**.
- **Distance calculation succeeded:**
  - **Only detected, no pending:** keep detected, **Sure**.
  - **Detected + pending:** detected + validated pending, **Neutral**.
  - **No detected, only pending:** use validated pending, **Neutral**.
  - Otherwise: keep what we have, confidence **Nothing** (or Unsure where applicable).

We also **do not** apply pending when **`pickupDropOutsideOfThreshold`** (we log and keep detected only).

Result is written back onto the **ride**:

```haskell
ride = updRide { tollCharges = tollCharges, tollNames = tollNames, tollIds = tollIds, tollConfidence = tollConfidence, ... }
```

So the ride now has the **merged** toll (detected + validated pending) and a confidence level.

---

## 5. How this flows into fare and DB

- **`recalculateFareForDistance`** (or the other `calculateFinalValuesFor*` paths) builds **fare params** with **`tollCharges = ride.tollCharges`** (the merged value above).
- **`newFareParams`** therefore includes the final toll (detected + pending).
- **`updRide'`** is set with **`tollCharges = mbUpdatedFareParams >>= (.tollCharges)`** so the ride row stores the same final toll that went into the fare.
- **`endRideTransaction`** (and related) persist the ride and run finance; **`clearTollStartGateBatchCache driverId`** (or equivalent) clears the **pending** Redis key so we don’t reuse it for the next ride.

So:

- **Recalculation** = one last run of snap-to-road + toll detection on (trip end points + any waypoints), or fallback to route/estimate.
- **Pending handling** = add charges only for tolls that were **estimated** but **not** in **detected**, and only if they appear in the **pending** cache and pickup/drop are within threshold.
- **Final toll** = detected + validated pending, with **Sure / Neutral / Unsure** as above, and that value is what goes into fare and ride.

---

**User**

What should be the flow of EndRide for SEPC logic (we want to keep it similar to toll), can you give me the flow for SEPC for endRide. Let me know the plan

---

**Cursor**

Here’s an **EndRide flow for SEPC** that mirrors the toll flow.

---

# EndRide flow for State Entry Permit Charges (SEPC)

## Goal

- Treat SEPC like tolls: **detected** (from route/waypoints) + **pending** (entry/crossing seen, “entry into permit state” not seen) + **estimate** (from search/booking).
- At end ride: run **final SEPC calculation** (or fallback), then **validate pending SEPC** using estimated SEPC IDs, **merge** detected + validated pending, set **confidence**, then use that in **fare** and **ride** and **clear** SEPC pending cache.

---

## 1. During ride (same idea as tolls)

- **Location batches** (or equivalent) are processed; for each batch we run **state-entry-permit detection** on the route/waypoints (e.g. areaBefore/areaAfter per segment, charge when `areaSecond` in SEPC and ≠ areaFirst).
- **Detected SEPC** (state permit IDs + amounts + names) are **accumulated** (e.g. Redis keys like `onRideStateEntryPermitChargesKey`, `onRideStateEntryPermitNamesKey`, `onRideStateEntryPermitIdsKey`) and periodically written to the **ride** row: `state_entry_permit_charges`, `state_entry_permit_names`, `state_entry_permit_ids`.
- When we see **“exit from a state”** (or “crossing”) but **no “entry into a known permit state”** in that batch, we treat it as **pending SEPC** and cache it (e.g. Redis key `stateEntryPermitPendingKey driverId`), similar to `tollStartGateTrackingKey` for tolls.

So by end ride we have:

- **Estimated SEPC:** `estimated_state_entry_permit_charges`, `estimated_state_entry_permit_names`, `estimated_state_entry_permit_ids` (from search/booking).
- **Detected SEPC:** `ride.state_entry_permit_charges`, `ride.state_entry_permit_names`, `ride.state_entry_permit_ids`.
- **Pending SEPC:** “We had a crossing but didn’t see entry into a permit state” (stored per driver in Redis).

---

## 2. End ride: high-level sequence (mirror tolls)

1. Run **final distance calculation** (existing flow that uses trip end points / waypoints).
2. Inside that, when toll logic runs, **also run SEPC logic** on the same waypoints/route so that **ride** gets the latest **detected** SEPC (and any new pending).
3. After that, **reload ride** → `updRide` has latest `state_entry_permit_charges/names/ids`.
4. Call **checkAndValidatePendingStateEntryPermits** (same idea as `checkAndValidatePendingTolls`).
5. **Merge** detected + validated pending; set **state_entry_permit_confidence** (Sure/Neutral/Unsure).
6. **Fare recalculation** uses merged SEPC (and toll) for `fareParams`; persist **ride** and **fare_parameters** with final SEPC; **clear SEPC pending cache** for the driver.

Below is the same flow in more detail.

---

## 3. Detailed EndRide flow for SEPC

### 3.1 Final distance calculation (existing + SEPC)

- **Input:** Same as today: `tripEndPoints`, `estimatedDistance`, and for SEPC: `estimatedStateEntryPermitCharges`, `estimatedStateEntryPermitNames`, `estimatedStateEntryPermitIds` (from booking/ride).
- **Behavior:**
  - **Path A (snap-to-road / waypoints run):** For each batch of waypoints, after distance/toll:
    - Run **SEPC detection** on the same route/points (RBB vs SBB, then in-memory areaBefore/areaAfter; charge when `areaSecond` in SEPC and ≠ areaFirst).
    - **Accumulate** SEPC in Redis (e.g. `onRideStateEntryPermitChargesKey`, names, ids) and append to **pending** when we see “crossing” but no “entry into known permit state”.
    - After each batch (when not failed), read Redis SEPC totals and call **updateStateEntryPermitChargesAndNamesAndIds** on the **ride** (same pattern as `updateTollChargesAndNamesAndIds`).
  - **Path B (no snap / fallback):** Use **getTravelledDistanceAndStateEntryPermitInfo** (or equivalent): e.g. search route + SEPC detection on that route, or fallback to **estimated** SEPC; then **updateStateEntryPermitChargesAndNamesAndIds** with that result.
- **Output:** Ride row has the latest **detected** SEPC (and Redis has any new pending).

So “recalculation” for SEPC = one last run of SEPC detection on the same data we use for toll/distance (waypoints or search route), plus fallback to estimate when we don’t run that.

---

### 3.2 Reload ride and validate pending SEPC

- After **finalDistanceCalculation** (and any other updates), **reload ride** from DB → **`updRide`**.
- Call:

  **`checkAndValidatePendingStateEntryPermits`**  
  (same contract as `checkAndValidatePendingTolls`):

  - **Inputs:**  
    `driverId`,  
    `estimatedStateEntryPermitCharges` (optional, for logging),  
    `estimatedStateEntryPermitNames` (optional),  
    **`estimatedStateEntryPermitIds`** (required for validation),  
    **`alreadyDetectedCharges`** = `updRide.state_entry_permit_charges`,  
    **`alreadyDetectedIds`** = `updRide.state_entry_permit_ids`.

  - **Logic:**
    - Read **pending SEPC** from Redis (e.g. `stateEntryPermitPendingKey driverId`).  
      Pending = “we had a crossing but no entry into a known permit state” (e.g. store state permit IDs we “might have entered” or a list of permit records we’re allowed to apply).
    - **Case 1:** We have pending, **estimated SEPC IDs**, and **detected SEPC IDs**.  
      - **Missing IDs** = estimated IDs − detected IDs.  
      - **Matching pending** = pending entries whose permit/state ID is in **missing IDs**.  
      - If any: return **`Just (charges, names, ids)`** to add to detected.  
      - Else: return **`Nothing`**.
    - **Case 2:** We have pending and estimated IDs, but **no** detected IDs (e.g. distance/SEPC detection failed).  
      - **Matching pending** = pending whose ID is in **estimatedStateEntryPermitIds**.  
      - If any: return **`Just (charges, names, ids)`** (apply estimated SEPC via pending).  
      - Else: **`Nothing`**.
    - **Case 3:** Pending present but **no** estimated SEPC IDs → return **`Nothing`** (don’t charge pending).
    - **Case 4:** No pending → **`Nothing`**.

  - **Output:** `Maybe (HighPrecMoney, [Text], [Text])` = extra charges, names, ids to add to detected.

---

### 3.3 Merge detected + validated pending and set confidence

- **Inputs:**  
  `updRide.state_entry_permit_charges/names/ids`,  
  **`mbValidatedPendingSEPC`** = result of **checkAndValidatePendingStateEntryPermits**,  
  **`pickupDropOutsideOfThreshold`** (and optionally **distanceCalculationFailed**), same as for tolls.

- **Rules (mirror tolls):**
  - **canApplyValidatedPendingSEPC** = `not pickupDropOutsideOfThreshold` (only apply pending when route is as expected).
  - **Distance calc succeeded:**
    - Only detected, no pending → keep detected; **state_entry_permit_confidence = Sure**.
    - Detected + pending (and `canApplyValidatedPendingSEPC`) → **merge** (detected + pending charges/names/ids); **confidence = Neutral**.
    - No detected, only pending (and `canApplyValidatedPendingSEPC`) → use validated pending; **confidence = Neutral**.
    - Else → keep current; confidence **Nothing** (or Unsure if you define it).
  - **Distance calc failed (or self-tuned):**
    - If we have estimated and detected: merge with pending when allowed → **Neutral**.
    - If we have estimated but no detected: use estimated or pending when allowed → **Neutral/Unsure** (same pattern as toll).
    - If no estimated: only apply pending when we have estimated IDs and `canApplyValidatedPendingSEPC` → **Unsure**; else no SEPC or **Nothing**.
  - **Never** apply pending when **pickupDropOutsideOfThreshold** (log and keep detected only).

- **Output:**  
  Final **`(stateEntryPermitCharges, stateEntryPermitNames, stateEntryPermitIds, stateEntryPermitConfidence)`** and set on **ride**:

  `ride = updRide { state_entry_permit_charges = ..., state_entry_permit_names = ..., state_entry_permit_ids = ..., state_entry_permit_confidence = ..., ... }`.

---

### 3.4 Fare recalculation and persistence

- **recalculateFareForDistance** (and other **calculateFinalValuesFor*** paths) already take **ride** and build **fare params**.
  - Extend **CalculateFareParametersParams** (or equivalent) with **stateEntryPermitCharges** (and names/ids if needed for breakup).
  - When building **FareParameters**, set **state_entry_permit_charges** (and optional names/ids) from **ride** (the merged value from step 3.3).
  - **Fare sum** already includes a separate “state entry permit” component (and you still send **tollCharges** to frontend = toll + SEPC for display).

- **updRide'** (final ride to persist):
  - Set **tollCharges** from fare params (as today).
  - Set **state_entry_permit_charges** (and names/ids/confidence) from the merged ride values (or from fare params if you store them there).  
  So ride row has final SEPC and confidence.

- **endRideTransaction** (or equivalent):
  - Persist **ride** and **fare_parameters** (both with final SEPC).
  - Call **clearStateEntryPermitPendingCache driverId** (same idea as **clearTollStartGateBatchCache**).

---

### 3.5 Cleanup

- On successful end ride (or when ride completes): **clear** Redis key(s) for SEPC pending for this driver (e.g. `stateEntryPermitPendingKey driverId`), so the next ride doesn’t reuse old pending SEPC.

---

## 4. Summary: SEPC EndRide flow (side-by-side with toll)

| Step | Toll | SEPC (plan) |
|------|------|-------------|
| 1. Final distance / waypoints | Run snap + toll on waypoints; update ride + Redis toll keys; pending in `tollStartGateTrackingKey` | Run same waypoints/route; run SEPC detection; update ride + Redis SEPC keys; pending in `stateEntryPermitPendingKey` |
| 2. Fallback when no snap | getTravelledDistanceAndTollInfo (route or estimate) | getTravelledDistanceAndStateEntryPermitInfo (route or estimate) |
| 3. Reload ride | updRide | updRide (with state_entry_permit_* from DB) |
| 4. Validate pending | checkAndValidatePendingTolls(estimatedTollIds, detectedTollIds) | checkAndValidatePendingStateEntryPermits(estimatedSEPCIds, detectedSEPCIds) |
| 5. Merge + confidence | Merge detected + pending; set tollConfidence (Sure/Neutral/Unsure) | Merge detected + pending; set stateEntryPermitConfidence |
| 6. Fare | fareParams.tollCharges from ride | fareParams.state_entry_permit_charges from ride; frontend gets tollCharges = toll + SEPC |
| 7. Persist | ride + fare_parameters; clearTollStartGateBatchCache | ride + fare_parameters; clearStateEntryPermitPendingCache |

---

## 5. Data you need on ride/booking

- **Ride:**  
  `state_entry_permit_charges`, `state_entry_permit_names`, `state_entry_permit_ids`, `state_entry_permit_confidence`,  
  and for estimate: `estimated_state_entry_permit_charges`, `estimated_state_entry_permit_names`, `estimated_state_entry_permit_ids` (if you want to mirror toll exactly).
- **Booking (or search request):**  
  Same estimated SEPC fields at init/search so that **checkAndValidatePendingStateEntryPermits** has **estimatedStateEntryPermitIds** (and optionally names/charges).

This is the flow and plan for SEPC at EndRide aligned with the existing toll flow.

---

**User**

"During ride (same idea as tolls)" do we have this case also, we recalucater only on endride right?

---

**Cursor**

We don’t recalculate only at end ride. Tolls (and the same idea for SEPC) are updated **during the ride** as well.

---

## During the ride

- The **location tracking** flow receives **waypoint batches** from the driver app.
- For each batch we run the **same pipeline** used at end ride: snap-to-road (when applicable) and **toll detection** on that batch’s route (`getTollInfoOnRoute merchantOpCityId (Just driverId) batchWaypoints`).
- When we get toll back we:
  - **Accumulate** in Redis (`onRideTollChargesKey`, `onRideTollNamesKey`, `onRideTollIdsKey`), and
  - Call **`updateTollChargesAndNamesAndIds`** → **ride** row in DB is updated with current `tollCharges`, `tollNames`, `tollIds`.
- If we see **entry but no exit** in that batch, we put that toll in the **pending** cache (Redis, `tollStartGateTrackingKey driverId`).

So **during the ride**, tolls are recalculated and written to the ride on **every** such batch, not only at end ride.

---

## At end ride

- We run **final distance calculation** with **trip end points** (one more run of the same pipeline).
- That can add a bit more detected toll and/or resolve pending tolls.
- Then we do **checkAndValidatePendingTolls** and merge detected + validated pending into the final toll and confidence.

So:

- **During ride:** Recalculate and update toll (and later SEPC) **per batch**; ride row is updated as we go.
- **End ride:** One **final** run on trip end points + **pending validation/merge** + fare and persistence.

For SEPC you’d mirror this: update **during ride** per batch (and maintain pending), then at **end ride** do the final run + validate pending + merge + fare. So “during ride” is a real case, not only end ride.

---

**User**

"So during the ride, tolls are recalculated and written to the ride on every such batch, not only at end ride." Which api does this

---

**Cursor**

The “during ride” toll (and distance) updates are driven by the **bulk location update** API.

---

## API: internal bulk location update

- **Module:** `API.Internal.BulkLocUpdate`
- **Route:** `"bulkLocUpdate"` under the internal API (e.g. **POST `/internal/bulkLocUpdate`** on the BPP, port 8016 in dev).
- **Handler:** `bulkLocUpdate` in `Domain.Action.Internal.BulkLocUpdate`.

**Request body (`BulkLocUpdateReq`):**

- `rideId` – current ride
- `driverId` – driver
- `loc` – **NonEmpty LatLong** (batch of waypoints)

**What it does:**

1. Loads ride, transporter config, booking.
2. Builds **RideInterpolationHandler** with `isEndRide = False` and batch size from config (`normalRideBulkLocUpdateBatchSize` / `meterRideBulkLocUpdateBatchSize`).
3. Calls **`addIntermediateRoutePoints`** with that handler and **`loc`** (the waypoints).
4. **`addIntermediateRoutePoints`** calls **`processWaypoints ih driverId False ... loc`** (`ending = False` → during-ride path):
   - Adds `loc` to the driver’s waypoints in Redis.
   - Runs **`recalcDistanceBatches`** (snap-to-road + **toll detection** per batch).
   - For each batch: **`interpolatePointsAndCalculateDistanceAndToll`** → **`getTollInfoOnRoute (Just driverId) batchWaypoints`**; result is accumulated in Redis and then **`updateTollChargesAndNamesAndIds`** updates the **ride** row.

So the API that recalculates and writes tolls (and distance) during the ride is:

**POST `/internal/bulkLocUpdate`**  
Body: `{ "rideId": "...", "driverId": "...", "loc": [ { "lat": ..., "lon": ... }, ... ] }`

It’s intended to be called by the **location tracking service** (or similar) whenever it has a new batch of driver locations for an active ride. `PriceBreakup.hs` also uses it (`BLoc.bulkLocUpdate`) when it has location updates to send.

---

**User**

So which files do you think will change? For this what is the file structure you are planning on?

---

**Cursor**

Here’s a concrete list of **files that would change** and the **file structure** for SEPC, based on how tolls are implemented and the flows we discussed.

---

## 1. New files (SEPC logic + types)

**Provider (dynamic-offer-driver-app) – shared SEPC logic (like TollsDetector):**

- **`Main/src/SharedLogic/StateEntryPermitDetector.hs`** (or `StateEntryPermitChargesDetector.hs`)  
  - Entry point: `getStateEntryPermitInfoOnRoute` (like `getTollInfoOnRoute`).  
  - Helpers: RBB vs SBB filter, fetch candidate geoms once, in-memory areaBefore/areaAfter, charge when `areaSecond` in SEPC and ≠ areaFirst, optional pending cache.  
  - `checkAndValidatePendingStateEntryPermits` (like `checkAndValidatePendingTolls`).  
  - Redis keys for pending SEPC and for on-ride accumulation (if you mirror toll Redis usage).

**Provider – optional small type module if you don’t put everything in the detector:**

- **`Main/src/Domain/Types/StateEntryPermitCharge.hs`**  
  - Only if you want a dedicated domain type for “one SEPC line” (id, amount, name, state, etc.); otherwise this can live in the detector or in types generated from YAML.

---

## 2. DB / YAML (namma-dsl → read-only code)

**New table:**

- **`Main/spec/Storage/StateEntryPermitCharge.yaml`**  
  - Defines `state_entry_permit_charge` (id, amount, city, state, merchant_operating_city_id, merchant_id, bounding_box or geom_id, etc.).  
  - After `, run-generator`: `src-read-only/Storage/Beam/StateEntryPermitCharge.hs`, `Storage/Queries/StateEntryPermitCharge.hs`, `Domain/Types/StateEntryPermitCharge.hs`, migrations.

**Existing tables – add SEPC columns (YAML only):**

- **`Main/spec/Storage/SearchRequest.yaml`** – e.g. `stateEntryPermitCharges`, `stateEntryPermitNames`, `stateEntryPermitIds` (and estimated if needed).
- **`Main/spec/Storage/Booking.yaml`** – same pattern.
- **`Main/spec/Storage/Ride.yaml`** – `state_entry_permit_charges`, `state_entry_permit_names`, `state_entry_permit_ids`, `state_entry_permit_confidence`, and estimated SEPC fields if you mirror toll.
- **`Main/spec/Storage/FareParameters.yaml`** – `stateEntryPermitCharges` (and optionally names/ids for breakup).
- **No** new columns on **FarePolicy** for SEPC (you’re dropping static toll from FarePolicy; SEPC is dynamic only).

**Merchant / allowed states (if new table):**

- If you introduce something like **`merchant_state` / `allowed_states`**, that would be a new YAML under `spec/Storage/` and generate its own Beam/Queries/Domain types.

**Geometry:**

- Geometry table already exists; StateEntryPermitCharge will reference it (e.g. `geom_id` FK). No change to Geometry YAML unless you add a new table that references it.

---

## 3. Provider – existing files to touch

**Search flow:**

- **`Main/src/Domain/Action/Beckn/Search.hs`**  
  - Where you today get `getTollInfoOnRoute` and build search request with tolls: add **getStateEntryPermitInfoOnRoute** (or call a combined “route charges” helper), and pass SEPC into **buildSearchRequest** (new params for SEPC charges/names/ids).  
  - **buildSearchRequest** signature and body: persist SEPC on search request (from YAML-generated fields).

**Init / Confirm / Booking:**

- **`Main/src/Domain/Action/Beckn/Init.hs`**  
  - When building booking from search request: copy `searchRequest.stateEntryPermitCharges` (and names/ids) onto booking, same way as `tollCharges`.
- **`Main/src/Domain/Action/Beckn/Confirm.hs`**  
  - Any place that builds trip quote / fare params with tolls: add SEPC (from booking or quote) into the same structures.

**Quote / Estimate / Fare:**

- **`Main/src/Domain/Action/Beckn/Search.hs`** (buildQuote / buildEstimate)  
  - Pass SEPC into **CalculateFareParametersParams** and into quote/estimate DTOs.
- **`Main/src/SharedLogic/FareCalculator.hs`**  
  - **CalculateFareParametersParams**: add `stateEntryPermitCharges` (and names/ids if needed).  
  - **FareParameters** (or the type that holds breakup): add SEPC fields.  
  - **pureFareSum** (or equivalent): include SEPC in total; optionally add a breakup line for SEPC.  
  - **mkFareParamsBreakups**: add SEPC breakup item when sending to BAP/frontend (and keep toll + SEPC separate in backend, clubbing only in the key sent to frontend if that’s the design).
- **`Main/src/SharedLogic/FarePolicy.hs`**  
  - Any breakup/build logic that today has toll: add SEPC line; ensure Beckn/API enums have a tag for SEPC if needed.
- **`Main/src/SharedLogic/SearchTry.hs`**  
  - **buildTripQuoteDetail**: add SEPC (from estimate/quote or fare policy fallback), same pattern as toll.

**Driver API / allocator:**

- **`Main/src/Domain/Action/UI/SearchRequestForDriver.hs`** (or equivalent “search request for driver” entity)  
  - Add SEPC to the entity sent to driver app (e.g. `stateEntryPermitCharges`, names/ids) if you show them; otherwise only ensure backend has them for fare.
- **`Main/src/SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/SendSearchRequestToDrivers.hs`**  
  - Where **buildTripQuoteDetail** is used: ensure SEPC is passed through so driver sees the same quote as backend.

**Update ride (edit destination):**

- **`Main/src/Domain/Action/Beckn/Update.hs`**  
  - Where you recalc route and toll for new destination: add **getStateEntryPermitInfoOnRoute** on the new route and feed SEPC into the updated fare/quote.

**Ride – during ride (bulk location):**

- **`Main/src/Lib/LocationUpdates.hs`**  
  - **getTravelledDistanceAndTollInfo**: add a parallel **getTravelledDistanceAndStateEntryPermitInfo** (or extend the existing function to return both toll and SEPC when you have a route).  
  - **buildRideInterpolationHandler**: pass a **getStateEntryPermitInfoOnRoute**-based callback and an **updateStateEntryPermitChargesAndNamesAndIds** so that when waypoints are processed we (a) run SEPC detection and (b) update ride + Redis (and pending cache) for SEPC.
- **`Main/src/Domain/Action/Internal/BulkLocUpdate.hs`**  
  - Already calls **addIntermediateRoutePoints**; that will internally use the handler that now does SEPC as well (no API change; handler built in LocationUpdates with SEPC support).
- **`Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs`**  
  - **RideInterpolationHandler**: add SEPC-related fields (e.g. `getStateEntryPermitInfoOnRoute`, `updateStateEntryPermitChargesAndNamesAndIds`).  
  - **interpolatePointsAndCalculateDistanceAndToll**: either extend to return SEPC as well or add a parallel “SEPC from route” step and merge.  
  - **recalcDistanceBatchStep** (and any place that calls **updateTollChargesAndNamesAndIds**): also call **updateStateEntryPermitChargesAndNamesAndIds** with SEPC from the same batch.  
  - **processWaypoints** (when `ending`): ensure SEPC is read from Redis (or equivalent) and written to ride, same as toll; and when `ending` is False, accumulate SEPC in Redis and update ride per batch.  
  - New Redis keys for on-ride SEPC accumulation and for pending SEPC (e.g. `onRideStateEntryPermitChargesKey`, `stateEntryPermitPendingKey`).

**Ride – end ride:**

- **`Main/src/Domain/Action/UI/Ride/EndRide.hs`**  
  - After **finalDistanceCalculation**, reload ride; call **checkAndValidatePendingStateEntryPermits** with estimated vs detected SEPC IDs.  
  - Compute merged SEPC and **state_entry_permit_confidence** (Sure/Neutral/Unsure) like toll.  
  - Set `ride.state_entry_permit_charges/names/ids/confidence`.  
  - In **recalculateFareForDistance** (and **calculateFinalValuesFor***): pass **ride.state_entry_permit_charges** (and names/ids if needed) into fare params.  
  - **updRide'**: set `state_entry_permit_charges` (and names/ids/confidence) from merged result.  
  - In **endRideTransaction** (or right after): call **clearStateEntryPermitPendingCache**.
- **`Main/src/Domain/Action/UI/Ride/EndRide/Internal.hs`**  
  - **endRideTransaction** / **processEndRideFinance**: any place that uses **tollCharges** for driver earnings or reporting: add **state_entry_permit_charges** the same way (if you treat SEPC as driver-pass-through like toll).  
  - **clearTollStartGateBatchCache**: add **clearStateEntryPermitPendingCache** (or a single “clear all ride caches” that includes both).

**Storage – ride updates:**

- **`Main/src/Storage/Queries/RideExtra.hs`** (or the editable Ride query file)  
  - **updateTollChargesAndNamesAndIds**: add **updateStateEntryPermitChargesAndNamesAndIds** (or extend the existing update to also set SEPC columns when provided).

**Beckn / BAP:**

- **`Main/src/SharedLogic/CallBAP.hs`**  
  - **sendRideCompletedUpdateToBAP**: when building fare breakup for BAP, add SEPC line (or fold into “toll” for frontend); ensure **fareParams** passed in includes SEPC.  
- **`Main/src/Beckn/OnDemand/Utils/OnUpdate.hs`** (or wherever ride_completed quote/breakup is built)  
  - Breakup list: add an item for state entry permit (and keep toll separate in backend); if frontend expects a single “tollCharges” key, sum toll + SEPC there.  
- **`Main/src/Beckn/ACL/Common/Order.hs`**  
  - **buildRideCompletedQuote**: ensure breakup includes SEPC (e.g. from **mkFareParamsBreakups** once fare params have SEPC).  
- **`Main/src/Beckn/OnDemand/Utils/Common.hs`**  
  - **filterRequiredBreakups** / tag enums: add state entry permit tag if you send it as a separate line in Beckn.

**Dashboard / merchant (optional):**

- **`Main/src/Domain/Action/Dashboard/Management/Merchant.hs`**  
  - Only if you expose SEPC or allowed_states in dashboard APIs (CRUD for StateEntryPermitCharge or merchant_state).  
- **`Main/spec/Storage/Merchant.yaml`** or **MerchantOperatingCity**  
  - Only if you add “allowed_states” or similar on an existing table; otherwise this is unchanged.

**Cancel / driver:**

- **`Main/src/SharedLogic/Cancel.hs`**  
  - **buildTripQuoteDetail**-style calls: add SEPC where toll is passed.  
- **`Main/src/Domain/Action/UI/Driver.hs`**  
  - Any driver-facing entity that shows toll: add SEPC or combined “toll” (toll + SEPC) per product decision.

---

## 4. Rider app (BAP) – if you store/display SEPC

- **Estimate / Quote types and storage**  
  - Where rider stores `tollCharges`/`tollChargesInfo`: add **stateEntryPermitCharges** (and names/ids) in backend; when sending to app, keep sending a single **tollCharges** = toll + SEPC.  
- **`Main/src/Domain/Action/Beckn/OnSearch.hs`**  
  - Parse SEPC from BPP item tags (if BPP sends them) and store on estimate/quote.  
- **`Main/src/Domain/Action/Beckn/OnStatus.hs`** (ride_completed)  
  - When building **fareBreakups** from BPP quote breakup: map “STATE_ENTRY_PERMIT” (or whatever tag) to a fare breakup row; still send **tollCharges** to frontend as sum of toll + SEPC if that’s the contract.  
- **Fare breakup storage**  
  - **fare_breakup** table: one row can be description = "TOLL_CHARGES" (or you add "STATE_ENTRY_PERMIT_CHARGES"); backend can store both and still expose one “tollCharges” to frontend.

(Exact rider file paths depend on your rider-app layout; the idea is: wherever toll is parsed, stored, or sent to the app, add SEPC in backend and keep a single toll key for the client.)

---

## 5. File structure summary

```text
Backend/app/provider-platform/dynamic-offer-driver-app/Main/
├── spec/Storage/
│   ├── StateEntryPermitCharge.yaml          # NEW
│   ├── SearchRequest.yaml                   # ADD columns
│   ├── Booking.yaml                         # ADD columns
│   ├── Ride.yaml                            # ADD columns
│   └── FareParameters.yaml                 # ADD columns
├── src/
│   ├── SharedLogic/
│   │   ├── StateEntryPermitDetector.hs      # NEW (like TollsDetector)
│   │   ├── FareCalculator.hs               # CHANGES (params, sum, breakup)
│   │   ├── FarePolicy.hs                    # CHANGES (breakup)
│   │   └── SearchTry.hs                     # CHANGES (trip quote SEPC)
│   ├── Domain/
│   │   ├── Action/
│   │   │   ├── Beckn/
│   │   │   │   ├── Search.hs                # CHANGES (SEPC on route, buildSearchRequest)
│   │   │   │   ├── Init.hs                  # CHANGES (booking SEPC)
│   │   │   │   ├── Confirm.hs               # CHANGES (quote/SEPC)
│   │   │   │   └── Update.hs                # CHANGES (SEPC on new route)
│   │   │   ├── UI/
│   │   │   │   ├── Ride/
│   │   │   │   │   ├── EndRide.hs           # CHANGES (pending, merge, confidence)
│   │   │   │   │   └── EndRide/Internal.hs   # CHANGES (clear cache, finance)
│   │   │   │   ├── Driver.hs                # CHANGES (if driver sees SEPC)
│   │   │   │   └── SearchRequestForDriver*  # CHANGES (entity with SEPC)
│   │   │   └── Internal/
│   │   │       └── BulkLocUpdate.hs         # no API change; handler gains SEPC via lib
│   │   └── Types/                           # optional StateEntryPermitCharge.hs if not from YAML
│   ├── Storage/Queries/
│   │   └── RideExtra.hs                     # CHANGES (updateStateEntryPermitChargesAndNamesAndIds)
│   ├── Lib/
│   │   └── LocationUpdates.hs               # CHANGES (SEPC in handler, getTravelled...SEPC)
│   ├── SharedLogic/
│   │   └── CallBAP.hs                       # CHANGES (ride_completed breakup with SEPC)
│   └── Beckn/
│       └── OnDemand/Utils/
│       └── ACL/Common/Order.hs              # CHANGES (breakup)
├── src-read-only/                           # generated from YAML (tables, Beam, OrphanInstances)
└── ...

Backend/lib/location-updates/
└── src/Lib/LocationUpdates/
    ├── Internal.hs                          # CHANGES (handler SEPC, Redis, processWaypoints)
    └── (Flow.hs or .hs that builds handler) # CHANGES (build handler with SEPC)
```

---

## 6. Minimal “first slice” (if you phase it)

- **Phase 1:** YAML + generator → StateEntryPermitCharge table + SearchRequest/Booking/Ride/FareParameters columns; **StateEntryPermitDetector.hs** with **getStateEntryPermitInfoOnRoute** only (no pending).  
- **Phase 2:** Search + Init + FareCalculator + buildQuote/buildEstimate + **recalculateFareForDistance** (so SEPC appears in fare and ride at end ride).  
- **Phase 3:** Location updates (BulkLocUpdate path + **finalDistanceCalculation**) + pending + **checkAndValidatePendingStateEntryPermits** + confidence + clear cache in EndRide.  
- **Phase 4:** BAP/breakup and rider-app (parse, store, send single tollCharges).

That’s the file set and structure that would change for SEPC end-to-end, with EndRide and “during ride” both covered in the same way as tolls.

---

**User**

Update ride (edit destination):


How is tol calculated here, we just call tolldetector file with updated polyline?

---

**Cursor**

In the **Update (edit destination)** flow, toll is calculated by calling **TollsDetector** with the **new route’s polyline** only. There’s no reuse of previous toll or during-ride accumulation.

What happens in code:

1. **New route is computed**  
   - For `SOFT_UPDATE`, `pickedWaypoints` is built (source + waypoints + new drop, including driver’s current location if ride is `IN_PROGRESS`).  
   - `Maps.getRoutes` is called with these waypoints.  
   - `shortestRoute` is taken from the response via `getRouteInfoWithShortestDuration` (so `shortestRoute.points` is the polyline of the chosen route to the new destination).

2. **Toll is computed on that polyline**  
   - TollsDetector is called with that polyline:

```256:256:Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Update.hs
            mbTollInfo <- getTollInfoOnRoute merchantOperatingCity.id (Just person.id) shortestRoute.points
```

So: **yes, we just call the TollsDetector with the updated polyline** (`shortestRoute.points`). No “previous” route or Redis accumulation is used here; it’s a one-shot: new route → `getTollInfoOnRoute(..., shortestRoute.points)`.

3. **How the result is used**  
   - **Validation:** `mbTollInfo` is used to decide if the vehicle type (e.g. auto rickshaw / bike) is allowed on that toll route; if not, the edit-destination request is rejected.  
   - **Fare:** The toll amount from that call is passed into fare calculation for the new destination:

```293:293:Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Update.hs
                    tollCharges = mbTollInfo <&> (\(tollCharges, _, _, _, _) -> tollCharges),
```

So for **SEPC in Update (edit destination)** you’d do the same:  
- Keep building `shortestRoute` as today.  
- Call a SEPC equivalent (e.g. `getStateEntryPermitInfoOnRoute(..., shortestRoute.points)`) with the same polyline.  
- Use that result for any vehicle/route checks and for `stateEntryPermitCharges` (and names/ids) in the new fare params / booking update request.

---

**User**

State Entry Permit Charges – Updated Implementation Plan
Things to always remember
DB changes: Make changes in the YAML file; run namma-dsl to autogenerate code.
Tolls: “Tolls” below = dynamic tolls only. Remove all static toll logic (e.g. FarePolicy.tollCharges).
State entry permit (SEPC): Implement in parallel to tolls – same tables/columns pattern, separate backend fields, clubbed on frontend.
Backend vs frontend: Backend keeps tollCharges and stateEntryPermitCharges (and names/IDs) separate; APIs send a single tollCharges key = sum of both.
Certainty: State permit at end ride uses Sure / Neutral / Unsure like tolls when location tracking fails or is partial.

1. StateEntryPermitCharges model (DB / YAML)
Table: StateEntryPermitCharges (state_entry_permit_charges)
id (PK)
amount (e.g. HighPrecMoney / Price)
city
state
merchant_operating_city_id
merchant_id
bounding_box: Store so RBB can be checked without loading geom. Options:
Option A: Array of 4 points (or 5 to close the rectangle), treat consecutive points as sides; or reuse existing BoundingBox type if present.
Option B: Store as geom in geometry table and reference it (consistent with rest of geo storage).
geom_id (or similar): Foreign key to Geometry table (the state boundary polygon used for containment and intersection).
Resolved (Engineering):
Bounding box: Either array of points (consecutive = sides) or a geom; align with how geojson/geom is stored elsewhere (e.g. geometry table). For RBB vs SBB you only need “sides” for intersection / within checks; existing getBoundingBox, lineSegmentWithinBoundingBox, doIntersect can be used.
Geometry table link: StateEntryPermitCharges has a column (e.g. geom_id) referencing Geometry table. Merchant_state / allowed_states link by state (and optionally city/region) to filter which SEPC rows (and thus which geoms) are candidates.

2. Flow (state entry permit calculation)
Step 1: Get all allowed_state of source from merchant_state.allowed_states (or equivalent).Step 2: Get all rows from StateEntryPermitCharges whose state is in the set from Step 1.Step 3: Get bounding box(es) of rows from Step 2 → SBB (one SBB per state, or union – same as your plan).Step 4: Build RBB = route polyline bounding box (e.g. getBoundingBox route).Step 5: For each candidate state’s SBB: check if any side of RBB intersects any side of SBB OR RBB is fully within SBB (e.g. all four corners of RBB inside SBB via pointWithinBoundingBox). Use existing helpers: doIntersect for segment–segment, pointWithinBoundingBox for “within”. If no intersection and not within → skip that state.Step 6 (candidate states only): For each state that passed Step 5:
Avoid per-point DB cost: Fetch that state’s geom (boundary polygon) once (or fetch all candidate state geoms in 1–K queries). Do not call “point within”/“contains” for every route point in the DB.
In-memory: Load polygon(s) into memory. For each consecutive pair (p1, p2) of the route polyline:
areaFirst = which state(s) contain p1 (using in-memory point-in-polygon on fetched geoms).
areaSecond = which state(s) contain p2 (same, in memory).
Overlap rule: If any of p1 or p2 lies in more than one geom (ambiguous), skip this segment and move to the next pair until you get a segment where both endpoints resolve to a single geom each (or one unknown). This avoids wrong attribution when boundaries overlap.
Charge rule: If areaSecond is in SEPC and areaSecond ≠ areaFirst, add areaSecond’s charge (and record name/id for that SEPC for names/IDs list). No need to “pair” exit with entry; charging on entry (second point) is enough.
Unknown: If areaFirst is known and areaSecond is “unknown” (no geom contains p2), do not charge; optionally mark “pending crossing” for end ride. If areaFirst is unknown and areaSecond is in SEPC, charge areaSecond (entry into permit state).
Step 7: If route segment does not intersect any candidate state boundary (or after applying overlap rule the segment is skipped), move to the next consecutive pair.Zig-zag / multiple entries: By iterating every pair and applying “areaSecond in SEPC and areaSecond ≠ areaFirst”, multiple entries into the same state (e.g. A→B→A→B) are charged each time (same as tolls).Entry/exit detection (resolved):
Do not infer “this is exit / this is entry” by sequence (e.g. “previous was exit so next is entry”). Use which area contains p1 and which area contains p2.
Exit = segment leaves a state (areaFirst = X, areaSecond ≠ X). Entry = segment enters a state (areaSecond = Y, areaFirst ≠ Y). We only need to charge on entry (areaSecond in SEPC).
If location tracking fails or we only see “exit” and never “entry into a known state”, handle at end ride via pending + estimated state permit IDs (same pattern as tolls: apply missing estimated SEPC with Neutral/Unsure).

3. Cases (Intracity / Cross City / Intercity)
Intracity – completely: Source, destination, and all polyline points in the same area. If no RBB side intersects any SBB side and RBB is not within any SBB, skip state permit calculation.
Intracity – partial: Source and destination in same area but polyline goes through another area. If RBB intersects or is within any SBB, run Step 6 (and in-memory pair logic above).
Cross city: Source and destination in different areas, destination in allowed states. If RBB intersects or is within any SBB, run Step 6.
Intercity: Source and destination in different areas, destination not in allowed states. If RBB intersects or is within any SBB, run Step 6.
No separate code paths per case; the same “RBB vs SBB → then in-memory pair + areaFirst/areaSecond” logic covers them.
4. Where state entry permit is stored (same as tolls)
Ride: e.g. state_entry_permit_charges, state_entry_permit_names, state_entry_permit_ids (and estimated equivalents if used for end-ride validation).
Search request / Booking: Same pattern for estimated state entry permit at search/init.
Fare params / breakup: Separate line item for state entry permit; names/IDs can be stored or exposed for debugging.
Account of SEPC (like toll names/IDs): Same tables that have toll_charges / toll_names / toll_ids get the corresponding state_entry_permit columns; frontend still receives one tollCharges = toll + state entry permit.

5. End ride & certainty (like tolls)
Sure: Full route available; all expected state entries detected; no pending.
Neutral: Some detected + some applied from validated pending (estimated state permit IDs that we didn’t see as “entry” but had pending crossing).
Unsure: Only estimated applied (e.g. tracking failed) or partial detection.
Pending: When we have “exited a state” (or crossed into unknown) but never saw “entry into a known permit state” in the data, set a pending state-permit crossing. At end ride, compare estimated state permit IDs (from search/booking) with detected state permit IDs; apply charges for missing IDs only if we have a pending crossing (and optionally pickup/drop within threshold, same as tolls). Do not charge pending without an estimate.
6. Engineering doubts – resolved
Bounding box for testing (geojson.io): Draw a rectangle (four points), export as GeoJSON. For DB: store either as array of points (consecutive = sides) or as geom; use same convention as existing geometry storage.
Bounding box storage: Either array of points (e.g. 4 or 5 points) or store as geom in geometry table; ensure RBB vs SBB can use existing getBoundingBox, lineSegmentWithinBoundingBox, doIntersect, pointWithinBoundingBox.
Four sides of RBB/SBB: From 4 corners you get 4 segments; consecutive corners form a side. Store SBB the same way (array or geom); no new core function needed.
Entry/exit: Use areaBefore (p1) and areaAfter (p2) from point-in-polygon; charge when areaSecond is in SEPC and areaSecond ≠ areaFirst. No exit–entry pairing.
Geometry table link: StateEntryPermitCharges has a FK (e.g. geom_id) to Geometry table; Merchant_state / allowed_states use state (and city/region if needed) to filter SEPC rows and thus which geoms to fetch.
Intersection / within: Use existing doIntersect, lineSegmentWithinBoundingBox, getBoundingBox, pointWithinBoundingBox.
Point-in-polygon: Use findGeometriesContainingGps / findGeometriesContaining only where it’s cheap (e.g. after fetching candidate geoms once). Prefer fetch geoms once → in-memory point-in-polygon for every route point to avoid per-point DB cost.
Overlapping state boundaries: If a point lies in more than one geom, skip that segment until you get a segment where each endpoint is in at most one geom (or one unknown). Then apply “areaSecond in SEPC and areaSecond ≠ areaFirst”.

7. Product
 So on backend these will be 2 separate fields toll charges and state entry permit charges, on fronted also in the api we will send these as 2 separate fields. Earlier I think we were clubbing Toll charges from fare policy to dynamic toll charges, since we will remove toll Charges from farepolicy, we will keep both dynamic toll charges and state permit entry charges separate on frontend.



8. Summary of approach changes from discussion
Charge rule: Whenever areaSecond is in SEPC and areaSecond ≠ areaFirst, add areaSecond’s charge. No requirement that “first” be in SEPC.
No exit–entry pairing: Use geometry (which area contains p1, which contains p2); do not use “previous was exit so next is entry”.
DB cost: Fetch candidate state geometries once (1–K queries); do point-in-polygon and pair logic in memory. If in-memory geom lib is not available, use one bulk DB query that returns which geom contains each route point.
Gaps: If areaSecond is “unknown”, don’t charge; use pending + estimated state permits at end ride.
Overlap: If either point is in multiple geoms, skip that segment; proceed when both endpoints resolve to at most one area each (or unknown).
Pending & certainty: Same pattern as tolls: pending when we saw exit (or unknown) but no entry into a known permit state; resolve at end ride using estimated state permit IDs with Sure/Neutral/Unsure.
This is the updated plan with nothing removed from your original structure and all discussed behaviour and doubt resolutions folded in.





Can you create an updated plan which is very detailed. Mentions each flow which needs to be changed (Search, Update, EndRide, Ride) etc. What is the exact logic in each please mention that as well. Try to keep it step wise, first start with removing existing changes like toll charges in farepolicy, then add db changes both postgres and redis (mention keys exactly and its need in plan), similar to tolls.


If at any point you are not able to decide what logic should be there refer toll logic for that same scenario, if you think toll logic can't be copied exactly let me know, I will try to answer it

---

**Cursor**

Exploring the codebase to map toll implementation and produce a detailed SEPC implementation plan.


Creating the detailed implementation plan.


---

**User**

"Step 5 – Filter by RBB vs SBB:

For each candidate state’s SBB: check if any side of RBB intersects any side of SBB or RBB is fully within SBB (existing doIntersect, pointWithinBoundingBox). If no intersection and not within, skip that state."


Why are we considering within, we just need to focus on those states which intersects, correct? Because we need to find out state entry charges of if there is no entry then 


"Validation at end ride: Implement checkAndValidatePendingStateEntryPermits (like checkAndValidatePendingTolls): compare estimated state permit IDs (from booking/ride) with detected IDs; apply charges for missing IDs only if we have a pending crossing and (optionally) pickup/drop within threshold. Do not charge pending without an estimate."

What are we going to do with these Pending SEPC because if exit is found and entry not found case will be in pending then on ride or on ride end the tolls be like that only or that will be even missing as in location update stopped and they did not come? This case won't be exaclt similar to tolls, what do you think?

---

**Cursor**

Good questions – both point to real differences from tolls.

---

## Question 1: "Within" in RBB vs SBB filter

You are correct to question it. For **tolls**, the check is: are any of the toll's `tollStartGates` (line segments) **within** the route's RBB? That makes sense because a gate is a narrow line segment that could be well inside the state's area.

For **SEPC**, the check is RBB (route bounding box) vs SBB (state bounding box). Here:

- **Intersection** (RBB sides vs SBB sides): the route's extent overlaps the state's extent → route might cross the boundary → meaningful.
- **RBB within SBB**: the entire route bounding box is inside the state's bounding box. This would be the intracity case – both pickup and drop are inside the state. In this case areaFirst and areaSecond will both always resolve to the same state in Step 6, so **no charge will ever be applied anyway**. The within check just causes extra work in Step 6 for no benefit.

So yes – for SEPC the "within" pre-filter is unnecessary. **Intersection only** is the right check: if the route's bounding box does not intersect the state's bounding box at all, the route cannot cross that state's border. If it does intersect, proceed to Step 6.

The "within" was probably inherited from a general toll bounding box pattern but doesn't carry meaning for border-crossing detection.

---

## Question 2: SEPC pending is NOT the same as toll pending

You are right that the mapping breaks down. Here is why:

**Toll pending (well-defined):**
- Entry gate detected (a route segment intersected `tollStartGates`), but the exit gate segment (`tollEndGates`) was not found in the current batch of waypoints.
- The driver is physically inside the toll zone. The next batch of waypoints will (usually) contain the exit gate.
- Redis stores the actual `Toll` objects. Next batch: look for exit gate first, if found → charge and clear.
- This is a genuine **in-progress, resumable state**: entry is certain, exit will come.

**SEPC "pending" – what would it actually mean?**

SEPC detection is per-pair: for (p1, p2), if `areaSecond` is in a permit state and `areaSecond ≠ areaFirst` → charge immediately. There is no "waiting for exit gate in next batch." Either the crossing pair exists in the waypoints or it doesn't. So there is no natural mid-ride pending like tolls.

The only cases where detection misses a real crossing are:

1. **GPS turned off / tracking failure mid-ride**: No waypoints covering the border crossing at all. `detectedStateEntryPermitIds` will be empty or partial. This is more like the toll `distanceCalculationFailed` path than the toll pending path.

2. **Ambiguous overlap / both points unknown**: Skipped segments due to overlap rule. The crossing happened but was skipped in detection.

3. **"Exited state but destination unknown" at end of a batch**: `areaFirst` = state A, `areaSecond` = unknown. We don't charge, we could note "possible crossing pending". But the next batch won't start with an exit-detection phase the way tolls do (because SEPC doesn't have exit gates at all). So there is no useful "resume in next batch" here. What actually helps is: at end ride, the driver **is** at the destination, which is in a known state – so if `areaSecond` was temporarily "unknown" but the final destination is in a permit state, that crossing will show up as detected anyway when we process the full route.

**What this means practically:**

The per-batch SEPC pending (mirroring `tollStartGateTrackingKey`) is **not needed**. The right model for SEPC is simpler:

- Accumulate `detectedStateEntryPermitIds` in Redis during ride (like `OnRideTollIds`).
- At end ride, compare detected IDs against estimated IDs (from booking/ride).
- If estimated IDs exist but some are missing from detected, and pickup/drop is within threshold (route was as expected), apply missing ones – this is Neutral confidence.
- If tracking completely failed (distance calculation failed), fall back to estimated SEPC entirely – this is Unsure.
- If all estimated IDs were detected with no issues – Sure.

No Redis "pending crossing to resume in next batch" is needed. The reconciliation is entirely at end ride using estimated vs detected, with confidence flags. The "pending" concept from tolls maps to the end-ride fallback logic, not to a batch-to-batch resumption.

So `StateEntryPermitPendingTracking:DriverId-<driverId>` key mentioned in the plan is likely unnecessary – the end-ride reconciliation (estimated IDs vs detected IDs + Confidence) handles everything.

---

**User**

Update the plan with these changes. Also have you mentioned on the API that we earlier we were just sending tollCharges in API response. Now we have to send both Toll Charges and stateEntry Permit Charges. 

Rest everything in plan is correct, so dont alter that please. Let it be detailed

---

**Cursor**

Updating the plan with the three changes: RBB vs SBB (intersection only), simplified SEPC end-ride validation (no per-batch pending), and explicit API contract (send both tollCharges and stateEntryPermitCharges).


---

**User**

Have you mentioned that in the plan that APIs that used to return just toll charges before we also need to return state entry permit charges as well in those apis on frontend

---

**Cursor**

Adding an explicit sentence: APIs that previously returned only toll charges must now also return state entry permit charges in those same APIs (backend and frontend).


Update made. In Phase 6, right after the “Change from current behaviour” line, this is now stated explicitly:

**“Every API that used to return only toll charges must now also return state entry permit charges in those same APIs – both on the backend (response payload) and on the frontend (where those responses are consumed and displayed).”**

So the plan now clearly says:
- Backend: same APIs that today return `tollCharges` must also return `stateEntryPermitCharges`.
- Frontend: wherever those APIs are used, the UI must handle and show both toll charges and state entry permit charges.

---

**User**

When mulitple bacthes come, there is an overlap between the last few waypoint of the current batch with first few batch of next point, how do we ensure in toll logic that a toll is not counter twice because of this? 




---

**Cursor**

Checking how batch waypoints are trimmed and how the toll result is accumulated:


Here's how the toll logic avoids double-counting even when batches overlap.

---

## 1. Overlap is one **point**, not segments

From [LocationUpdates/Internal.hs](Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs):

- Each step takes **one batch**: `getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)` (e.g. first 100 points).
- After processing, it drops the first N points: `deleteFirstNwaypoints driverId maxSnapToRoadReqPoints` (e.g. first 99).

So the next batch starts from the **last point of the previous batch**. Concretely:

- **Batch 1** segments: `(p0,p1), (p1,p2), …, (p98,p99)`  
- After delete: points left are `p99, p100, …`  
- **Batch 2** segments: `(p99,p100), (p100,p101), …, (p197,p198)`

So every segment `(p_i, p_{i+1})` appears in **exactly one** batch. The overlap is only the **point** `p99`; the segment `(p98,p99)` is only in batch 1, and `(p99,p100)` only in batch 2. No segment is processed twice.

---

## 2. A toll is charged only when **exit** is found

From [TollsDetector.hs](Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/TollsDetector.hs):

- The detector walks the route segment by segment. For each segment it checks **entry** (intersection with `tollStartGates`) and then looks **forward** on the same route for **exit** (intersection with `tollEndGates`).
- **If both entry and exit are found in the same batch:** the toll is added once and the route is “sliced” after the exit; processing continues. That toll is never considered again.
- **If entry is in this batch but exit is not:** the toll is **not** added. It is stored in **pending** (`tollStartGateTrackingKey`). The code returns **without** adding any charge for it (lines 154–162).

So a toll is only **counted** at the moment its **exit** is found, and that happens on one specific segment in one specific batch.

---

## 3. Next batch: resolve **pending** first, then new entries

When the **next** batch is processed, the detector is called with `mbDriverId = Just driverId`. Then (lines 186–191):

1. It loads **pending** tolls from Redis (`tollStartGateTrackingKey`).
2. It runs **only exit resolution** for those pending tolls on the **current batch’s route**: `getAggregatedTollChargesConsideringStartSegmentFromPrevBatchWhileOnRide` → `getExitTollAndRemainingRoute route tollCombinationsWithStartGatesInPrevBatch`.
3. The “route” here is the **current batch’s points** (e.g. `p99..p198`). So it looks for the **exit** segment in this batch. It does **not** re-check entry.
4. When an exit is found, that toll is added **once**, and removed from pending (lines 204–206).

So:

- The segment that had **entry** was in the previous batch and is not in the current batch (no segment overlap).
- The segment that has **exit** is in the current batch and is processed once.
- So that toll is added exactly once when its exit is found.

---

## 4. Pending is de-duplicated by toll ID

When adding to pending (lines 156–161), the code merges with existing pending and then:

```haskell
uniquePendingTolls = nubBy (\toll1 toll2 -> getId toll1.id == getId toll2.id) allPendingTolls
```

So the same toll (same id) is not stored twice in pending, which would otherwise risk double-counting if exit were found in two different batches (it isn’t, but this is an extra safeguard).

---

## Summary

| Mechanism | Role in avoiding double count |
|----------|-------------------------------|
| **Batch boundary** | `deleteFirstNwaypoints` keeps overlap to a single **point**. Every segment `(p_i, p_{i+1})` is in exactly one batch. |
| **Charge only on exit** | A toll is added only when its exit segment is found. Entry alone only puts it in pending. |
| **Pending + next batch** | Next batch only looks for **exit** for pending tolls on the **current** route. The entry segment is not re-processed. |
| **nubBy on pending** | Same toll id is not duplicated in pending. |

So the overlap between batches is intentional (one shared point for continuity), but because no **segment** is ever in two batches and charging happens only when exit is found (once), no toll is counted twice.

---

**User**

For sateEntryPermitCharges also we might have to store pending then because lets say the crossing point is last point of first batch and first point of next batch, so we wont be checking it if we dont store pending sepc?

---

**Cursor**

Checking how the route is built for the toll/SEPC call and whether the batch boundary can drop the segment between the last point of one batch and the first of the next.


You’re right to worry about the batch boundary. Here’s how it works today and why SEPC (and toll) can miss the boundary segment, and how to fix it.

---

## What actually gets passed to the detector

From [LocationUpdates/Internal.hs](Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs):

1. **Batch N:** `batchWaypoints = getFirstNwaypoints(driverId, 99)` → e.g. points `[p0..p98]`.
2. These are sent to **snap-to-road**; we get **this batch’s** snapped points only: `response.snappedPoints` (e.g. `[S0..S98]`).
3. **Toll (and later SEPC)** is run on **only** those points: `getTollInfoOnTheRoute(Just driverId, response.snappedPoints)` (line 483).

So:

- **Batch 1** gets segments `(S0,S1), (S1,S2), …, (S97,S98)`.
- **Batch 2** gets a **new** snapped list for the next 99 raw points, e.g. `[S99', S100', …]`, and segments `(S99',S100'), (S100',S101'), …`.

The segment **(S98, S99')** — last point of batch 1 and first point of batch 2 — is **never** passed to the detector in either batch. So:

- Any **toll** whose entry or exit lies on that “boundary” segment can be missed.
- Any **SEPC** crossing that happens between S98 and S99' (one side last of batch 1, other side first of batch 2) will also be missed.

So yes: for SEPC we **do** need to do something at the batch boundary; and the same gap exists for toll on the boundary segment.

---

## Two ways to fix it (for SEPC and optionally toll)

### Option A: “Pending” style (store last point, check boundary segment)

- After each batch, store the **last snapped point** (and for SEPC optionally “area” for that point).
- At the **start** of the next batch, before running the rest of the route:
  - Form the **one** segment: `(storedLastPoint, firstPointOfCurrentBatch)`.
  - Run toll/SEPC logic **only** for that segment (toll: entry/exit check; SEPC: area(last) vs area(first), charge if first is in SEPC and ≠ last).
- Then run the detector on the **full** current batch as today (so we don’t double-count segments inside the batch).

So we’re not “pending” in the sense of “exit not found”; we’re “carrying over” one point so the **boundary segment** is checked. For SEPC that’s exactly “store pending [last point] and then check it when the next batch arrives.”

### Option B: Prepend previous last point to current batch (continuous route)

- After each batch, store the **last snapped point** (e.g. in Redis, key per driver).
- When processing the **next** batch, form the route as:  
  **`storedLastPoint : currentBatchSnappedPoints`**  
  and call `getTollInfoOnTheRoute` / `getStateEntryPermitInfoOnRoute` on this list.

Then:

- The first segment the detector sees is **(storedLastPoint, firstCurrent)** = the boundary segment, so no crossing is lost.
- All other segments are still in exactly one “batch” (no duplicate segments), so no double-counting.

So we **don’t** need a separate “pending SEPC” concept for the boundary; we only need to **prepend** the previous batch’s last point so the route is continuous. Same idea can be used for toll so the boundary segment is never missed.

---

## Summary

- **Yes:** For state entry permit charges we **do** have to handle the case where the crossing is between the last point of one batch and the first point of the next; otherwise that crossing is never checked.
- **Ways to do it:**
  - **Pending-style:** Store last point (and optionally area for SEPC), then explicitly check the single segment `(last, first_of_next_batch)` before running the rest of the batch. That’s “store pending SEPC [last point] and check it when the next batch comes.”
  - **Prepend:** Store last snapped point; prepend it to the next batch’s snapped points before calling the detector. Then the boundary segment is naturally included and we don’t need a separate pending concept for the boundary.

So your point is correct: we **do** need something (either “pending” boundary point or prepending) so that the segment between the last point of the first batch and the first point of the next batch is checked; otherwise we can miss a state-boundary crossing for SEPC (and the same gap exists for toll on that segment).

---

**User**

In case of tolls it checks if any consecutive segment intersects the exit of any toll and is exit corresponding to any pending tolls stored? correct?

---

**Cursor**

Yes. That’s how the toll logic works.

From [TollsDetector.hs](Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/TollsDetector.hs):

1. **When we have a driver (on-ride)** we first load **pending** tolls (entry seen, exit not yet seen):
   ```haskell
   mbTollCombinationsWithStartGatesInPrevBatch <- Hedis.safeGet (tollStartGateTrackingKey driverId)
   ```
   If that’s non-empty we call `getAggregatedTollChargesConsideringStartSegmentFromPrevBatchWhileOnRide` with the **current batch’s route** and those **pending** tolls.

2. **Exit check for pending tolls** is done by walking the **current route segment by segment** and seeing if any segment is an **exit** for any of those pending tolls:
   ```haskell
   getExitTollAndRemainingRoute route tollCombinationsWithStartGatesInPrevBatch
   ```
   So we’re asking: “Does any consecutive segment `(p1, p2)` in this route intersect the **exit gates** of any of the **pending** tolls?”

3. **Inside `getExitTollAndRemainingRoute`** (lines 125–129):
   - It walks `(p1 : p2 : ps)`.
   - For each segment it checks: does `(LineSegment p1 p2)` intersect **any** of that toll’s `tollEndGates`?
   - The `tolls` argument here is exactly the **pending** tolls (same entry gate, exit not yet found).
   - So it’s: “Is this segment an exit for **any** of the pending tolls?”

So yes: we **do** check whether **any consecutive segment** in the current batch’s route intersects the **exit** of **any** of the **pending** tolls, and that exit is interpreted as “exit corresponding to” one of those pending tolls. When we find such a segment we charge that toll once and remove it from pending.

---

**User**

So how should it be handled in case of state entry permit charges?

---

**Cursor**

For State Entry Permit Charges (SEPC), the batch-boundary case should be handled by **making the route continuous** at the boundary, not by adding an entry/exit-style “pending” like tolls.

---

## How SEPC is different from tolls

- **Tolls:** Entry and exit are two different segments. We can see entry in one batch, store “pending,” and in the next batch look for a segment that intersects the **exit** of that pending toll.
- **SEPC:** The crossing is on a **single** segment `(p1, p2)`: we charge when `areaSecond` is in SEPC and `areaSecond ≠ areaFirst`. There is no “exit” to match later; we just need that segment to be evaluated.

So we don’t need “pending SEPC” in the toll sense. We only need the **boundary segment** `(last point of batch 1, first point of batch 2)` to be passed to the SEPC detector.

---

## Recommended approach: prepend previous batch’s last point

Handle SEPC (and optionally toll) at the batch boundary by **prepending the previous batch’s last snapped point** to the current batch’s route before calling the detector.

**1. Store last snapped point after each batch**

- After each batch, persist the **last snapped point** of that batch (e.g. Redis key like `LastSnappedPointForTollSepc:<driverId>`).
- Value: that one `LatLong` (or equivalent). TTL similar to other on-ride keys (e.g. 6h).

**2. When running SEPC (and optionally toll) on the next batch**

- Get current batch’s snapped points: `currentSnapped = response.snappedPoints` (e.g. `[S99', S100', ..., S197']`).
- If we have a stored **last snapped point** from the previous batch (e.g. `S98`), form:
  - **Route for detector:** `storedLastPoint : currentSnapped`  
  - i.e. `[S98, S99', S100', ..., S197']`.
- Call `getStateEntryPermitInfoOnRoute(..., storedLastPoint : currentSnapped)` (and, if you fix the boundary for toll too, `getTollInfoOnRoute(..., storedLastPoint : currentSnapped)`).
- After the call, update the stored last point to the **last** point of the current batch (e.g. `S197'`), so the next batch can use it.

**3. First batch**

- There is no “previous batch,” so no stored point. Use the current batch’s snapped points as-is. Don’t set a stored point until after the first batch has been processed.

**4. Why this is enough for SEPC**

- The detector walks **every consecutive pair** of the route and applies the rule “areaSecond in SEPC and areaSecond ≠ areaFirst.”
- The first pair is now `(storedLastPoint, firstPointOfCurrentBatch)` = the boundary segment, so any state-boundary crossing there is seen and charged once.
- All other segments are unchanged; no segment is processed twice, so no double-counting.
- No SEPC-specific “pending” or “exit matching” logic is needed; the only change is **which route** we pass in (continuous across batches by prepending one point).

---

## Summary

| Aspect | Tolls | SEPC (recommended) |
|--------|--------|---------------------|
| Boundary segment | Currently can be missing (same as SEPC). Can be fixed by same prepend. | Include by **prepending** previous batch’s last snapped point to current batch. |
| Pending (entry/exit) | Yes: entry in one batch, look for exit in next. | Not needed: crossing is one segment; prepend gives that segment. |
| Implementation | Keep existing pending; optionally add prepend to close boundary gap. | **Only** prepend: store last snapped point, prepend before calling SEPC (and toll if desired). |

So for state entry permit charges, handle the batch boundary by **storing the last snapped point per batch and prepending it to the next batch’s snapped route** before calling the SEPC detector; no separate “pending SEPC” state or exit-matching logic is required.

---

**User**

For tolls do we have logic to handle that duplicate tolls are not counted, lets say there is toll counted in last few points while on ride and the same points are repeated in next few point in the next batch. What if a toll is in this common point between 2 batch, it might be counted twice

---

**Cursor**

Checking how the route is sliced after a toll is found and whether the same toll passage can be processed in two batches:


Here’s how the current toll logic avoids double-counting and why “common points” don’t make the same toll count twice.

---

## 1. Segments, not points, matter

- A toll is **charged** when we see **entry** on some segment `(p_i, p_{i+1})` and **exit** on a later segment `(p_j, p_{j+1})`.
- So what can be “counted twice” is the **same passage** (same entry segment + same exit segment).

---

## 2. No segment is in two batches

- Batch 1: waypoints `[p0..p98]` → segments `(p0,p1), …, (p97,p98)`.
- After the step we call `deleteFirstNwaypoints(98)`, so the next batch starts at index 98.
- Batch 2: waypoints `[p98, p99, …, p196]` → segments `(p98,p99), (p99,p100), …, (p195,p196)`.

So:

- The **only** overlap between batches is the **point** `p98` (last of batch 1, first of batch 2).
- The **segment** `(p97, p98)` is only in batch 1.
- The **segment** `(p98, p99)` is only in batch 2.

So every segment exists in **at most one** batch. The “common point” is just one point; the two segments that touch it are different and live in different batches. So the same toll passage (entry on one segment, exit on another) cannot be “seen” in two different batches.

---

## 3. How the code prevents double-count

**When entry and exit are in the same batch**

- We find entry on segment `(p_i, p_{i+1})`, then find exit on a later segment in the **same** route.
- We add the toll **once** and **slice the route** at the exit: we continue from `remainingRoute` (line 144), so that exit segment is never processed again.
- That toll is **not** put in pending.

**When entry is in one batch and exit in the next**

- Batch 1: we see entry, don’t see exit → we put that toll in **pending** (and we **don’t** add it to charges).
- Batch 2: we **first** look for an **exit** for **pending** tolls only (`getExitTollAndRemainingRoute route tollCombinationsWithStartGatesInPrevBatch`). When we find a segment that intersects the **exit** of a pending toll we add that toll **once** and **remove it from pending** (`removeMatchedTollFromCache`).
- Then we process the **remaining** route for new tolls. The segment where we just found the exit is consumed (we continue from `remainingRoute`), so it is not used again as “entry” or “exit” in the same run.

So:

- Each toll passage is either completed in one batch (entry + exit, add once, slice route) or completed across two batches (entry → pending, next batch only looks for **exit** for that pending toll, add once, remove from pending).
- We never re-process the same segment in another batch, so the same passage can’t be completed twice.

**Pending de-duplication**

- When adding to pending we merge with existing and then:
  - `uniquePendingTolls = nubBy (\toll1 toll2 -> getId toll1.id == getId toll2.id) allPendingTolls`
- So the same toll (same id) is not stored multiple times in pending, which would otherwise be the only way to “match exit” more than once for the same logical passage.

---

## 4. “Toll in the common point” / boundary

- If the **exit** lies on the boundary (between last point of batch 1 and first of batch 2), that segment is **not** in either batch today, so we might **miss** that exit (toll stays pending or is resolved at end ride). So the risk at the boundary is **under**-counting, not over-counting.
- If we **prepend** the previous batch’s last point to the next batch’s route to fix that, we are **adding** the single boundary segment `(last_prev, first_curr)`. That segment is new; it wasn’t part of any batch before. So we don’t duplicate any segment and we don’t double-count; we only make it possible to see a toll whose exit (or entry) falls on that boundary.

---

## Summary

- **Yes:** the logic is designed so the same toll passage is not counted twice.
- **Why:** (1) each segment is in at most one batch, (2) a toll is charged only when we see exit (in the same batch or for a pending toll in the next), (3) after charging we remove that toll from pending and slice the route so that exit segment isn’t reused, (4) pending is de-duplicated by toll id.
- The “common point” between batches is only one point; the two segments that touch it are in different batches, so the same toll is not completed in both batches. There is no explicit “duplicate toll” check because the batching and pending/exit logic already ensure each passage is completed at most once.

---

**User**

"Batch 1: waypoints [p0..p98] → segments (p0,p1), …, (p97,p98).
After the step we call deleteFirstNwaypoints(98), so the next batch starts at index 98.
Batch 2: waypoints [p98, p99, …, p196] → segments (p98,p99), (p99,p100), …, (p195,"


Where is the code for it?

---

**Cursor**

Here’s where that behaviour comes from in code.

---

## 1. Reading the first N waypoints (batch)

**File:** [Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs](Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs)

**Implementation** – take first `num` elements from the Redis list (indices `0 .. num-1`):

```423:424:Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs
getFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m [LatLong]
getFirstNwaypointsImplementation driverId num = lRange (makeWaypointsRedisKey driverId) 0 (num - 1)
```

**Use in the batch step** – read **99** points (`maxSnapToRoadReqPoints + 1`):

```282:282:Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs
  batchWaypoints <- getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)
```

So for each batch we get waypoints at indices `0..98` of the **current** list (e.g. first batch: `[p0..p98]`).

---

## 2. Deleting the first N waypoints after the batch

**Implementation** – trim the list so only indices `numToDel` onward remain (Redis `LTRIM`):

```429:430:Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs
deleteFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m ()
deleteFirstNwaypointsImplementation driverId numToDel = lTrim (makeWaypointsRedisKey driverId) numToDel (-1)
```

**Use in the batch step** – delete the first **98** points (only when not meter ride):

```294:294:Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs
  unless isMeterRide $ deleteFirstNwaypoints driverId maxSnapToRoadReqPoints -- delete the batch irrespective of the snapToRoadFailed
```

So after the step, the list in Redis starts at the old index 98 (one point overlap with the batch we just processed).

---

## 3. Where 98 comes from

Handler is built in [Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Lib/LocationUpdates.hs](Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Lib/LocationUpdates.hs):

```288:291:Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Lib/LocationUpdates.hs
  return $
    mkRideInterpolationHandler
      (fromMaybe transportConfig.normalRideBulkLocUpdateBatchSize mbBatchSize) -- keeping batch size of 98 by default for bulk location updates to trigger snapToRoad
      98
```

So:

- **batchSize** = config or default (e.g. 98).
- **maxSnapToRoadReqPoints** = **98** (second argument to `mkRideInterpolationHandler`).

Hence:

- **Read:** `getFirstNwaypoints driverId (98 + 1)` → 99 points, indices 0..98.
- **Delete:** `deleteFirstNwaypoints driverId 98` → drop indices 0..97, keep from 98 onward.

So the “Batch 1: [p0..p98], then delete first 98, Batch 2: [p98, p99, …]” behaviour is exactly from these three places: the two implementations and the single batch step that uses them with `maxSnapToRoadReqPoints + 1` and `maxSnapToRoadReqPoints`.

---

**User**

@.cursor/plans/state_entry_permit_charges_implementation_41fa27d8.plan.md Based on this discussion regarding handling the batch points what do you think need to be updated in the plan? 

---

**Cursor**

Reading the plan and adding the batch-boundary handling we discussed.


Adding a subsection on batch-boundary handling: new Redis key, Phase 4 note on continuous route, and Phase 5.5 prepend-and-store steps.


---

**User**

You mentioned for the toll flow the next segment's first point is same as previous segment last point. So the case where a segment is not considered will never come correct?

---

**Cursor**

Yes. With the prepend approach, that case goes away.

**Without prepend (current gap):**
- Batch 1 gets snapped points [S0..S98] → detector sees segments (S0,S1) … (S97,S98). Segment (S98, S99) is never seen.
- Batch 2 gets [S99..S197] → detector sees (S99,S100) … (S196,S197). Again (S98, S99) is never seen.
- So the boundary segment (last of batch N, first of batch N+1) is never passed to the detector.

**With prepend:**
- Batch 1: same as above; then we store last point S98.
- Batch 2: we prepend S98 → route is [S98, S99, …, S197]. Detector sees (S98,S99), (S99,S100), …, (S196,S197).
- So (S98, S99) is the first segment of the continuous route for batch 2 and is considered.

Once we implement “store last snapped point after each batch” and “prepend it to the next batch’s snapped points before calling the detector,” the route is continuous and every segment between consecutive points is considered, including the boundary. So the “segment not considered” case (the batch-boundary gap) does not occur anymore. Same idea applies for both toll and SEPC detectors.

---

**User**

In case of toll which approach is followed?

---

**Cursor**

Checking how the toll flow handles batch boundaries in the location-updates code.


In the current code, **toll does not use the prepend approach**. It has the same batch-boundary gap as we described for SEPC.

What the code does:

1. **Batch waypoints**  
   `recalcDistanceBatchStep` gets only this batch’s waypoints:
   - `batchWaypoints <- getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)` (line 282).

2. **Snap and toll**  
   `interpolatePointsAndCalculateDistanceAndTollImplementation`:
   - Calls snap-to-road with `wps` (current batch) and `distanceCalcReferencePoint` (used only for **distance** via `calculateDistanceFrom`, not for building the toll route).
   - Calls the toll detector with **only** the current batch’s snapped points:
   - `getTollInfoOnTheRoute (Just driverId) response.snappedPoints` (lines 483–484).

So the toll detector never sees the segment between the last point of batch N and the first point of batch N+1. There is no Redis “last snapped point” for toll, and no prepending.

**Summary**

| Aspect | Current toll flow |
|--------|--------------------|
| Route passed to toll detector | Current batch’s snapped points only |
| Boundary segment (last_N, first_N+1) | Not passed to detector (same gap as SEPC) |
| Prepending previous batch’s last point | Not done |

So today **both** toll and SEPC can miss a crossing at the batch boundary. The **prepend approach** (store last snapped point, prepend to next batch before calling detectors) is the **planned** fix for **both** toll and SEPC in the location-updates flow; it is not implemented for toll yet. The plan’s batch-boundary continuity section applies to toll and SEPC together.

---

**User**

"1. Reading the first N waypoints (batch)
File: Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs
Implementation – take first num elements from the Redis list (indices 0 .. num-1):
Internal.hs
Lines 423-424
getFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m [LatLong]
getFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m [LatLong]getFirstNwaypointsImplementation driverId num = lRange (makeWaypointsRedisKey driverId) 0 (num - 1)
Use in the batch step – read 99 points (maxSnapToRoadReqPoints + 1):
Internal.hs
Lines 282-282
  batchWaypoints <- getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)
So for each batch we get waypoints at indices 0..98 of the current list (e.g. first batch: [p0..p98]).
2. Deleting the first N waypoints after the batch
Implementation – trim the list so only indices numToDel onward remain (Redis LTRIM):
Internal.hs
Lines 429-430
deleteFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m ()deleteFirstNwaypointsImplementation driverId numToDel = lTrim (makeWaypointsRedisKey driverId) numToDel (-1)
Use in the batch step – delete the first 98 points (only when not meter ride):
Internal.hs
Lines 294-294
  unless isMeterRide $ deleteFirstNwaypoints driverId maxSnapToRoadReqPoints -- delete the batch irrespective of the snapToRoadFailed
So after the step, the list in Redis starts at the old index 98 (one point overlap with the batch we just processed).
3. Where 98 comes from
Handler is built in Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Lib/LocationUpdates.hs:
LocationUpdates.hs
Lines 288-291
  return $    mkRideInterpolationHandler      (fromMaybe transportConfig.normalRideBulkLocUpdateBatchSize mbBatchSize) -- keeping batch size of 98 by default for bulk location updates to trigger snapToRoad      98
So:
batchSize = config or default (e.g. 98).
maxSnapToRoadReqPoints = 98 (second argument to mkRideInterpolationHandler).
Hence:
Read: getFirstNwaypoints driverId (98 + 1) → 99 points, indices 0..98.
Delete: deleteFirstNwaypoints driverId 98 → drop indices 0..97, keep from 98 onward.
So the “Batch 1: [p0..p98], then delete first 98, Batch 2: [p98, p99, …]” behaviour is exactly from these three places: the two implementations and the single batch step that uses them with maxSnapToRoadReqPoints + 1 and maxSnapToRoadReqPoints."


Then what was this appraoch which you mentioned earlier, here the last and first point of 2 batch are same right?

---

**Cursor**

You’re right to focus on the overlap.

**Raw waypoints are continuous (1-point overlap)**  
- Batch 1: read 99 → `[p0..p98]`  
- Delete first 98 → list becomes `[p98, p99, ...]`  
- Batch 2: read 99 → `[p98, p99..p196]`  

So in **raw** space, the last point of batch 1 and the first point of batch 2 are the **same** point (p98). There is no “gap” in the raw route.

**Where the gap is: snapped points**

The toll (and future SEPC) detector is not called on raw waypoints. It’s called on **snapped** points only:

- Batch 1: snap `[p0..p98]` → get `[S0, S1, ..., S98]` → detector sees segments `(S0,S1)..(S97,S98)`.
- Batch 2: snap `[p98..p196]` → get `[S98', S99', ..., S196']` → detector sees `(S98',S99')..(S195',S196')`.

So we have:

- **Raw:** p98 is shared → continuous.
- **Snapped:** Batch 1 ends at **S98** (snapped from p98 in context of batch 1). Batch 2 starts at **S98'** (snapped from p98 in context of batch 2). They can differ (different API call, context, or floating point) → the **segment (S98, S98')** is never passed as a consecutive pair in any single detector call.

So:

- If **S98 = S98'** (same point): that “segment” is zero length; no crossing can be missed.
- If **S98 ≠ S98'**: the segment between them is real and we never run the detector on it → possible missed toll/SEPC at the batch boundary.

So the “approach” that **does** have last = first is the **raw** waypoint batching (overlap p98). The **detector** instead runs on **per-batch snapped** lists, so the boundary **snapped** segment (S98, S98') can be missing. The prepend approach fixes that by making the detector see one continuous snapped route that includes that boundary segment (by prepending the previous batch’s last **snapped** point to the next batch’s snapped list).

---

**User**

For toll detetctor where is the code for this where batches are prcessed, give me code snippet and how they are processed?

---

**Cursor**

Here’s where batches are processed and how the toll detector is called.

---

## 1. Batch loop (location-updates)

Batches are driven from `recalcDistanceBatches` in **Internal.hs**. When `snapToRoadCallCondition && isAtLeastBatchPlusOne` holds, it calls `processSnapToRoadCall`, which runs `recalcDistanceBatches'` and keeps calling `recalcDistanceBatchStep` while there are enough waypoints.

**`Internal.hs`** – batch loop and single batch step:

```228:244:Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs
    recalcDistanceBatches' isFirstCall snapToRoad'@SnapToRoadState {..} snapToRoadCallFailed = do
      batchLeft <- continueCondition
      if (batchLeft && not isMeterRide) || (batchLeft && isMeterRide && isFirstCall)
        then do
          (dist, newReferencePoint, startPatching, servicesUsed, snapCallFailed) <- recalcDistanceBatchStep h isMeterRide distanceCalcReferencePoint rectifyDistantPointsFailureUsing isTollApplicable driverId
          -- ... state update ...
          else pure (snapToRoad', snapToRoadCallFailed)
```

So: **one batch** = one call to `recalcDistanceBatchStep`; it’s repeated until `continueCondition` is false (no full batch left).

---

## 2. One batch: read waypoints → snap → toll → accumulate → delete

**`Internal.hs`** – `recalcDistanceBatchStep`:

```273:304:Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs
recalcDistanceBatchStep RideInterpolationHandler {..} isMeterRide distanceCalcReferencePoint rectifyDistantPointsFailureUsing isTollApplicable driverId = do
  -- ... meter-ride patching (reference point) ...
  batchWaypoints <- getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)
  (distance, interpolatedWps, servicesUsed, snapToRoadFailed, mbTollChargesAndNames) <- interpolatePointsAndCalculateDistanceAndToll distanceCalcReferencePoint' rectifyDistantPointsFailureUsing isTollApplicable driverId batchWaypoints
  whenJust mbTollChargesAndNames $ \(tollCharges, tollNames, tollIds, _, _) -> do
    void $ Redis.rPushExp (onRideTollNamesKey driverId) tollNames 21600
    void $ Redis.rPushExp (onRideTollIdsKey driverId) tollIds 21600
    void $ Redis.incrby (onRideTollChargesKey driverId) (round tollCharges.getHighPrecMoney)
    Redis.expire (onRideTollChargesKey driverId) 21600 -- 6 hours
  whenJust (nonEmpty interpolatedWps) $ \nonEmptyInterpolatedWps -> do
    addInterpolatedPoints driverId nonEmptyInterpolatedWps
  -- ...
  unless isMeterRide $ deleteFirstNwaypoints driverId maxSnapToRoadReqPoints
  -- ...
  let newReferencePoint = if snapToRoadFailed then Nothing else lastMaybe interpolatedWps
  pure (distance, newReferencePoint, startPatching, servicesUsed, snapToRoadFailed)
```

So for each batch:

1. **Read:** `getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)` → 99 raw waypoints (e.g. `[p0..p98]` then `[p98..p196]` after trim).
2. **Snap + toll:** `interpolatePointsAndCalculateDistanceAndToll ... batchWaypoints` → distance, **snapped** points, and toll result for **this batch only**.
3. **Accumulate:** push toll names/ids and increment toll charges in Redis.
4. **Trim:** `deleteFirstNwaypoints driverId maxSnapToRoadReqPoints` so the next batch is the next 99 raw points.

---

## 3. Where the toll detector is called (on snapped points only)

**`Internal.hs`** – `interpolatePointsAndCalculateDistanceAndTollImplementation`:

```474:486:Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs
interpolatePointsAndCalculateDistanceAndTollImplementation isEndRide snapToRoadCall getTollInfoOnTheRoute distanceCalcReferencePoint rectifyDistantPointsFailureUsing isTollApplicable driverId wps = do
  if isEndRide && isAllPointsEqual wps
    then pure (0, take 1 wps, [], False, Nothing)
    else do
      (servicesUsed, res) <- snapToRoadCall rectifyDistantPointsFailureUsing $ Maps.SnapToRoadReq {points = wps, distanceUnit = Meter, calculateDistanceFrom = distanceCalcReferencePoint}
      case res of
        Left _ -> pure (0, [], [], True, Nothing)
        Right response -> do
          tollInfo <-
            if isTollApplicable
              then getTollInfoOnTheRoute (Just driverId) response.snappedPoints
              else return Nothing
          pure (response.distance, response.snappedPoints, servicesUsed, False, tollInfo)
```

So:

- **Input to this batch:** raw `wps` (e.g. 99 waypoints).
- **Snap:** `snapToRoadCall` returns `response.snappedPoints` for **this batch only**.
- **Toll:** `getTollInfoOnTheRoute (Just driverId) response.snappedPoints` → toll detector sees **only this batch’s snapped points**, no prepended point from the previous batch.

---

## 4. Provider: who implements `getTollInfoOnTheRoute`

**`LocationUpdates.hs`** (provider):

```301:302:Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Lib/LocationUpdates.hs
      (TollsDetector.getTollInfoOnRoute merchantOpCityId)
```

So `getTollInfoOnTheRoute` is exactly `TollsDetector.getTollInfoOnRoute merchantOpCityId` with arguments `(Just driverId)` and `response.snappedPoints`.

---

## 5. Toll detector (batch-aware “pending” tolls)

**`TollsDetector.hs`** – entry point and handling of pending tolls across batches:

```179:210:Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/TollsDetector.hs
getTollInfoOnRoute merchantOperatingCityId mbDriverId route = do
  tolls <- B.runInReplica $ findAllTollsByMerchantOperatingCity merchantOperatingCityId
  if not $ null tolls
    then do
      let boundingBox = getBoundingBox route
          eligibleTollsThatMaybePresentOnTheRoute = filter (...) tolls
      case mbDriverId of
        Just driverId -> do
          mbTollCombinationsWithStartGatesInPrevBatch :: Maybe [Toll] <- Hedis.safeGet (tollStartGateTrackingKey driverId)
          case mbTollCombinationsWithStartGatesInPrevBatch of
            Just tollCombinationsWithStartGatesInPrevBatch -> getAggregatedTollChargesConsideringStartSegmentFromPrevBatchWhileOnRide driverId ...
            Nothing -> getAggregatedTollCharges route eligibleTollsThatMaybePresentOnTheRoute (0, [], [])
        Nothing -> getAggregatedTollCharges route ...
    else return Nothing
```

So:

- **`route`** here is always **one batch’s snapped points** (e.g. `[S0..S98]` or `[S98'..S196']`), because that’s what Internal passes.
- For **on-ride** (`Just driverId`), it uses **`TollGateTracking:DriverId-<driverId>`** to carry “entry seen in a previous batch, exit not yet seen” and resolves those in `getAggregatedTollChargesConsideringStartSegmentFromPrevBatchWhileOnRide` using the **current** batch’s route.
- There is **no** prepending of the previous batch’s last snapped point; so the segment between last point of batch N and first point of batch N+1 is never passed as a single continuous route to the detector.

---

## Summary

| Step | Where | What happens |
|------|--------|----------------|
| Batch loop | `Internal.hs` `recalcDistanceBatches'` | While `continueCondition`: call `recalcDistanceBatchStep`. |
| Read batch | `recalcDistanceBatchStep` | `getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)` → 99 raw waypoints. |
| Snap + toll | `interpolatePointsAndCalculateDistanceAndTollImplementation` | Snap current batch → `getTollInfoOnTheRoute (Just driverId) response.snappedPoints` (this batch’s snapped points only). |
| Accumulate | `recalcDistanceBatchStep` | Push names/ids, incr charges in Redis. |
| Trim | `recalcDistanceBatchStep` | `deleteFirstNwaypoints driverId maxSnapToRoadReqPoints` so next batch is the next 99 raw points. |
| Toll detector | `TollsDetector.getTollInfoOnRoute` | Receives **only** current batch’s snapped route; uses Redis for “entry in previous batch, find exit in this batch”. No prepend of previous batch’s last snapped point. |

---

**User**

Read: getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1) → 99 raw waypoints (e.g. [p0..p98] then [p98..p196] after trim).

here p98 is overlapping right, so we dont miss any segment?

---

**Cursor**

Yes. In **raw** space we don’t drop any segment because of the overlap.

- **Batch 1:** raw points `[p0..p98]` → segments `(p0,p1) .. (p97,p98)`.
- **After trim:** list is `[p98, p99, ...]`.
- **Batch 2:** raw points `[p98..p196]` → segments `(p98,p99), (p99,p100) .. (p195,p196)`.

So the segment **(p98, p99)** is the first segment of batch 2. Every consecutive raw pair is covered; the overlap (p98 in both batches) is what keeps the raw route continuous.

Where the nuance is:

- The **toll/SEPC detector** is called on **snapped** points, not raw.
- Batch 1: snap `[p0..p98]` → `[S0..S98]` → detector sees `(S0,S1)..(S97,S98)`.
- Batch 2: snap `[p98..p196]` → `[S98', S99', ..]` → detector sees `(S98',S99'), (S99',S100')..`.

So:

- The segment that corresponds to **(p98, p99)** **is** passed to the detector: it’s **(S98', S99')** in batch 2. So we do **not** miss that segment.
- The only thing we never pass is the **virtual** segment **(S98, S98')** — i.e. from “last snapped point of batch 1” to “first snapped point of batch 2.” That’s zero length if the snap of p98 is the same in both batches; if the snap differs slightly between the two calls, it’s a tiny segment that could theoretically be missed.

**Short answer:**  
- **Raw:** overlap of p98 means we don’t miss any raw segment.  
- **Snapped:** we still don’t miss (p98,p99); it’s seen as (S98',S99'). The only possible gap is the tiny (S98, S98') link if the two snap results for p98 differ.

---

**User**

@Internal.hs (1-523) 


Explain the whole flow, what comes in this function what is called by it and what input is given to toll calculating function and how does it handle batches

---

**Cursor**

Here’s the flow in one place: what enters, what gets called, what the toll function gets, and how batching works.

---

## 1. Entry: `processWaypoints`

**Where:** Lines 107–131  

**Input:**  
- `RideInterpolationHandler` (built by provider with `mkRideInterpolationHandler`),  
- `driverId`, `ending`, `estDist`, `estTollCharges` / `estTollNames` / `estTollIds`,  
- flags: `pickupDropOutsideThreshold`, `rectifyDistantPointsFailureUsing`, `isTollApplicable`, `enableTollCrossedNotifications`, `passedThroughDrop`, `isMeterRide`,  
- **`waypoints`** – `NonEmpty LatLong` (new locations for this update).

**What it does:**  
- Wraps in `wrapDistanceCalculation` (catches errors, sets “distance calc failed” in Redis).  
- Pushes `waypoints` to edit-destination list and to the main waypoints list via `addPoints driverId waypoints`.  
- Calls **`recalcDistanceBatches`** with the same params (so waypoints are already in Redis when recalc runs).

---

## 2. Top-level recalc: `recalcDistanceBatches`

**Where:** Lines 167–262  

**Input:** Same as above (handler, driverId, ending, est*, flags, waypoints, etc.).  

**What it does:**

1. **Continuity for deviation**  
   - Reads `lastTwoOnRidePointsRedisKey driverId` (last two points of previous update).  
   - Builds `modifiedWaypoints = prevTwo <> waypoints` for route deviation.  
   - Writes new “last two points” from current `waypoints` back to Redis.

2. **Route and toll deviation**  
   - `updateRouteDeviation driverId (toList modifiedWaypoints)` → `(routeDeviation, tollRouteDeviation, isTollPresentOnCurrentRoute)` (provider-supplied).

3. **Optional notifications**  
   - If `isTollApplicable && enableTollCrossedNotifications && isTollPresentOnCurrentRoute`, forks toll-crossed notification and BAP update.

4. **`processPassedThroughDrop`**  
   - Updates `SnapToRoadState.passThroughDropThreshold` in Redis.

5. **Branch on `ending`**
   - **If `ending` (ride end):**
     - If **`snapToRoadCallCondition`**: runs **`processSnapToRoadCall`** (processes all full batches), then `updateDistance` and flushes accumulated toll from Redis to ride (`updateTollChargesAndNamesAndIds`).
     - Else: uses **`getTravelledDistanceAndTollInfo`** (estimated distance/toll) and updates distance and toll on ride.
   - **If not ending:**
     - If `getWaypointsNumber driverId > batchSize` and **`snapToRoadCallCondition`**: runs **`processSnapToRoadCall`**, then `updateDistance` and stores updated `SnapToRoadState` in Redis.

So the **batch processing** and **toll calculation** only run when `snapToRoadCallCondition` is true and (for non-ending) there are more than `batchSize` waypoints.

---

## 3. Batch loop: `processSnapToRoadCall` → `recalcDistanceBatches'`

**`processSnapToRoadCall`** (246–254):  
- Loads previous `SnapToRoadState` from Redis (or default).  
- Calls **`recalcDistanceBatches' True prevSnapToRoadState False`**.  
- On snap failure, updates distance and throws.

**`recalcDistanceBatches'`** (229–244):  
- **`continueCondition`**: if `ending` then “any waypoints left”, else “waypoints > batchSize” (i.e. at least one full batch).  
- While there is a “batch left” (and meter-ride / first-call conditions allow it):
  - Calls **`recalcDistanceBatchStep h isMeterRide distanceCalcReferencePoint ... isTollApplicable driverId`** (no waypoints arg – it reads from Redis).
  - Uses the result to update `SnapToRoadState` (distance, reference point, service counts, etc.).
  - Repeats with the new state until no full batch remains.

So one “snap-to-road call” can run **many** `recalcDistanceBatchStep` calls in a loop until the waypoint list is drained to below a full batch.

---

## 4. Single batch step: `recalcDistanceBatchStep`

**Where:** Lines 264–307  

**Input:**  
- `RideInterpolationHandler`, `isMeterRide`, `distanceCalcReferencePoint`, `rectifyDistantPointsFailureUsing`, `isTollApplicable`, `driverId`.  
- No waypoints argument; waypoints come from Redis.

**What it does:**

| Step | Code / effect |
|------|----------------|
| 1. Meter-ride patching | If `isMeterRide`: may trim waypoints to last 98 and set `distanceCalcReferencePoint'` so distance is patched across batches. |
| 2. **Read batch** | `batchWaypoints <- getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)` → **99 raw points** (e.g. indices 0..98). |
| 3. **Snap + distance + toll** | `interpolatePointsAndCalculateDistanceAndToll distanceCalcReferencePoint' ... driverId batchWaypoints` → `(distance, interpolatedWps, servicesUsed, snapToRoadFailed, mbTollChargesAndNames)`. |
| 4. **Accumulate toll in Redis** | If `mbTollChargesAndNames` is present: `rPush` names/ids to `onRideTollNamesKey` / `onRideTollIdsKey`, `incrby` charges on `onRideTollChargesKey`. |
| 5. **Store snapped points** | `addInterpolatedPoints driverId interpolatedWps` (append snapped points to Redis). |
| 6. Snap failure fallback | If snap failed: append raw `batchWaypoints` as interpolated. |
| 7. **Trim waypoints** | `deleteFirstNwaypoints driverId maxSnapToRoadReqPoints` → drop first **98** elements; list starts at previous index 98 (one-point overlap for next batch). |
| 8. Persist toll to ride | If snap didn’t fail and `isTollApplicable`: read accumulated toll from Redis and call **`updateTollChargesAndNamesAndIds`** (provider: writes to ride). |
| 9. Reference for next batch | `newReferencePoint = last interpolatedWps` (for next batch’s distance patching). |

So each batch: **read 99 raw → snap → toll on this batch’s snapped points → accumulate toll in Redis → trim 98 raw waypoints → (if not failed) persist current accumulated toll to ride.**

---

## 5. Where toll is computed: `interpolatePointsAndCalculateDistanceAndTollImplementation`

**Where:** Lines 457–486  

**Input:**  
- `isEndRide`, `snapToRoadCall`, **`getTollInfoOnTheRoute`** (from handler),  
- `distanceCalcReferencePoint`, `rectifyDistantPointsFailureUsing`, `isTollApplicable`, `driverId`,  
- **`wps`** – raw waypoints for **this batch only** (the 99 just read).

**What it does:**

1. **Short-circuit**  
   - If `isEndRide && isAllPointsEqual wps`: return zero distance, single point, no toll.

2. **Snap to road**  
   - `snapToRoadCall ... (SnapToRoadReq { points = wps, ..., calculateDistanceFrom = distanceCalcReferencePoint })`  
   - Gets `response` with `response.distance` and **`response.snappedPoints`** (snapped version of `wps`).

3. **Toll**  
   - On success:  
     - **`getTollInfoOnTheRoute (Just driverId) response.snappedPoints`**  
   - So the **toll function** is called with:
     - **First argument:** `Just driverId` (on-ride; detector can use driver-scoped Redis for “entry in previous batch, exit in this one”).  
     - **Second argument:** **`response.snappedPoints`** – **only this batch’s snapped points**, not the full route.

4. **Return**  
   - `(response.distance, response.snappedPoints, servicesUsed, False, tollInfo)`.

So: **input to the toll-calculating function is always “this batch’s snapped points” plus `Just driverId`.** Raw waypoints overlap (e.g. p98 in two batches), but the detector only ever sees per-batch snapped lists.

---

## 6. How batches are handled (end-to-end)

- **Waypoints**  
  - Stored in Redis at `waypoints:driverId` (list).  
  - New points are appended by `addPoints` in `processWaypoints`.

- **Batch size**  
  - Handler is built with `maxSnapToRoadReqPoints = 98` (and a `batchSize`, e.g. 98).  
  - **Read:** `getFirstNwaypoints driverId (98+1)` → 99 elements (indices 0..98).  
  - **Delete after step:** `deleteFirstNwaypoints driverId 98` → keep from index 98 onward.  
  - So: batch 1 = [p0..p98], then list = [p98,p99,...]; batch 2 = [p98..p196], etc. One-point overlap in **raw** space.

- **Per batch:**  
  - Raw batch (99 points) → snap → **toll on snapped points only** → toll result accumulated in Redis and (when not failed) written to ride; then 98 raw points removed from the front.

- **Toll “across” batches:**  
  - The **toll detector** (e.g. `TollsDetector.getTollInfoOnRoute`) is invoked once per batch with **only that batch’s `response.snappedPoints`**.  
  - It gets `Just driverId`, so it can use Redis (e.g. `TollGateTracking:DriverId-...`) to remember “entry gate seen in a previous batch” and resolve exit in a later batch.  
  - So batching is handled by: (a) calling the toll function once per batch with that batch’s snapped route, and (b) the detector itself maintaining cross-batch state in Redis when `driverId` is present.

---

## 7. Flow diagram (concise)

```
processWaypoints(handler, driverId, ending, est*, …, waypoints)
  → addPoints(waypoints)   // append to Redis list waypoints:driverId
  → recalcDistanceBatches(...)

recalcDistanceBatches
  → updateRouteDeviation(modifiedWaypoints)
  → if (ending or (waypoints > batchSize)) && snapToRoadCallCondition
      → processSnapToRoadCall

processSnapToRoadCall
  → recalcDistanceBatches'(state) in a loop

recalcDistanceBatches'
  → while (waypoints > batchSize or waypoints > 0 when ending):
      → recalcDistanceBatchStep

recalcDistanceBatchStep
  → batchWaypoints = getFirstNwaypoints(99)     // [p0..p98] then [p98..p196] …
  → (distance, snapped, …, mbToll) = interpolatePointsAndCalculateDistanceAndToll(batchWaypoints)
  → accumulate mbToll in Redis (names, ids, charges)
  → addInterpolatedPoints(snapped)
  → deleteFirstNwaypoints(98)
  → updateTollChargesAndNamesAndIds(accumulated from Redis)

interpolatePointsAndCalculateDistanceAndToll(wps)
  → response = snapToRoadCall(wps)
  → tollInfo = getTollInfoOnTheRoute(Just driverId, response.snappedPoints)   // input to toll = this batch’s snapped points only
  → return (distance, snapped, …, tollInfo)
```

So: **what comes in** is `waypoints` and handler/context; **what’s called** is the chain above; **input to the toll function** is **`(Just driverId, this batch’s snapped points)`**; **batches** are handled by reading 99, deleting 98, and the toll detector using Redis to carry entry/exit state across those batches.

---

**User**

Does toll detector store all detected toll ids in redis for a ride, what if redis goes down?

---

**Cursor**

Checking what the toll detector and location-updates flow store in Redis vs DB and what happens if Redis is unavailable.


Here’s how toll data is stored and what happens if Redis is down.

---

## 1. Does the toll detector store all detected toll IDs in Redis for a ride?

**Yes.** During the ride, toll data is accumulated in Redis. Two separate mechanisms are involved:

### A. Location-updates (Internal.hs) – accumulated toll per driver

| Redis key | Type | Purpose |
|----------|------|--------|
| `OnRideTollCharges:<driverId>` | Counter | Sum of toll charges (updated with `incrby` each batch) |
| `OnRideTollNames:<driverId>` | List | Toll names (append with `rPush` each batch) |
| `OnRideTollIds:<driverId>` | List | Toll IDs (append with `rPush` each batch) |

So **all detected toll IDs (and names/charges) for that ride are stored in these Redis keys** as batches are processed. They are also written to the **ride row in Postgres** after each batch and at ride end (see below).

### B. TollsDetector – pending tolls across batches

| Redis key | Type | Purpose |
|----------|------|--------|
| `TollGateTracking:DriverId-<driverId>` | Serialized `[Toll]` | Tolls where entry was seen in a previous batch but exit not yet found |

So:

- **All detected toll IDs** for the ride are stored in Redis in `OnRideTollIds:<driverId>` (and names/charges in the other two keys).
- **Pending** tolls (entry seen, exit not yet seen) are stored in `TollGateTracking:DriverId-<driverId>`.

---

## 2. Persistence to DB (ride table)

Redis is the **accumulation buffer**; Postgres is the **persistent record** for the ride.

- After each batch (in `recalcDistanceBatchStep`): the code **reads** from Redis (`safeGet` for charges, `lRange` for names/ids) and calls **`updateTollChargesAndNamesAndIds`**.
- That function (**RideExtra.hs** 370–379) updates the **Ride** row in Postgres:

```370:379:Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Queries/RideExtra.hs
updateTollChargesAndNamesAndIds driverId tollCharges tollNames tollIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamR.tollCharges (Just tollCharges),
      Se.Set BeamR.tollNames (Just tollNames),
      Se.Set BeamR.tollIds (Just tollIds),
      ...
    ]
    [Se.And [Se.Is BeamR.driverId (Se.Eq $ getId driverId), Se.Is BeamR.status (Se.Eq Ride.INPROGRESS)]]
```

So **detected toll IDs are stored in Redis during the ride and repeatedly synced to the ride table in Postgres**. At end ride, the final toll (including any validated pending tolls) is again written to the ride and used for fare.

---

## 3. What if Redis goes down?

There is **no special “Redis down” path**. Behavior is:

### Writes to Redis (accumulation / pending)

- **Location-updates:** `rPushExp`, `incrby`, `expire` on the OnRide toll keys will **fail** if Redis is unavailable → the monad will throw, and the whole distance/toll calculation for that update can fail.
- **TollsDetector:** `Hedis.setExp (tollStartGateTrackingKey ...)` (storing/updating pending tolls) will also fail if Redis is down.

So if Redis is down, **any step that tries to write to Redis can cause the request to fail** (e.g. location update or batch step failing).

### Reads from Redis

- **`safeGet` / `lRange`:** If the Hedis layer is written to return `Nothing` / empty on Redis errors, then:
  - Reading OnRide toll keys would yield “no accumulated toll” → `updateTollChargesAndNamesAndIds` would be called with **empty/zero** (or whatever the code passes in that case), **overwriting** the ride’s toll in Postgres with that empty/zero state.
  - So you can **lose** previously persisted toll data for that ride if Redis is down when you read.
- **TollsDetector:** `Hedis.safeGet (tollStartGateTrackingKey driverId)` would return `Nothing` → no pending tolls from previous batches → possible **missing or double-counted** tolls at batch boundaries, and no validation of pending tolls at end ride.

### End ride

- Final flush of OnRide keys would again read from Redis; if Redis is down you get nothing/empty and the ride may be closed with no or wrong toll.
- `checkAndValidatePendingTolls` uses Redis for pending tolls; if Redis is down it effectively sees no pending tolls.

### Summary if Redis goes down

| Aspect | Effect |
|--------|--------|
| **Storing detected toll IDs** | Accumulation in Redis fails → request can fail; no new toll data in Redis. |
| **Ride table (Postgres)** | If reads return empty, DB can be updated with 0 / [] and **override** previously saved toll for that ride. |
| **Pending tolls (cross-batch)** | Not stored / not read → possible missed or wrong toll at boundary; end-ride validation of pending tolls is ineffective. |
| **Recovery** | No built-in recovery; Redis is required for correct on-ride toll accumulation and for pending-toll handling. |

So: **yes, the toll detector (and location-updates) store all detected toll IDs in Redis for the ride**, and they are also written to the ride in Postgres. **If Redis goes down, you can get request failures, lost or overwritten toll data on the ride, and incorrect handling of pending tolls**, with no dedicated fallback in the current design.

---

**User**

So for sepc we also need to add to ride extra as well?

---

**Cursor**

Yes. For SEPC you need the same kind of ride persistence that toll has, so you do need to add something in the ride storage layer (e.g. RideExtra or the same module where ride fields are updated).

**Toll today:**

- **RideExtra.hs** has `updateTollChargesAndNamesAndIds` (and the Ride table has `tollCharges`, `tollNames`, `tollIds`).
- On-ride: accumulate in Redis, then after each batch (and at ride end) **read from Redis and call** `updateTollChargesAndNamesAndIds` so the **Ride** row in Postgres stays in sync.

**SEPC should mirror that:**

1. **Ride table (Postgres)**  
   Add columns for SEPC on the ride (e.g. `stateEntryPermitCharges`, `stateEntryPermitNames`, `stateEntryPermitIds`, and any confidence field if you have one). That’s the “Postgres schema for SEPC in parallel to tolls” from the plan.

2. **Ride storage / “ride extra”**  
   In the same place where you have `updateTollChargesAndNamesAndIds` (e.g. **RideExtra.hs**), add an equivalent for SEPC, e.g.  
   `updateStateEntryPermitChargesAndNamesAndIds :: Id Person -> HighPrecMoney -> [Text] -> [Text] -> m ()`  
   that updates those new ride columns (and `updatedAt`), same pattern as the toll update.

3. **On-ride flow**  
   After each batch (and at ride end), read the accumulated SEPC from Redis and call this new update so the ride row has the latest detected SEPC, just like toll.

So yes: for SEPC you need to add the corresponding **update function in RideExtra** (or the same ride-update module) and call it from the on-ride flow whenever you persist accumulated SEPC to the ride.

---

**User**

but we read only from redis, not the ride extra table when we need to fetch the tolls?

---

**Cursor**

Yes. In the **on-ride location-updates / batch flow** we use **only Redis** when we need the current toll state; we don’t read it from the Ride table there.

**In `recalcDistanceBatchStep` (Internal.hs):**

- After each batch we **read** toll from Redis:
  - `Redis.safeGet (onRideTollChargesKey driverId)`
  - `Redis.lRange (onRideTollNamesKey driverId) 0 (-1)`
  - `Redis.lRange (onRideTollIdsKey driverId) 0 (-1)`
- With that we call **`updateTollChargesAndNamesAndIds`**, which **writes** to the Ride row in Postgres.

So in that flow we **never read** `tollCharges` / `tollNames` / `tollIds` from the Ride table; we only read from Redis and then **write** to the Ride table. RideExtra is just the **persistence target**, not the source we read from during batching.

**When the Ride table is read for toll:**  
Whenever the ride is loaded for something else (e.g. end ride, ride-detail API), the Ride entity from DB already has the latest toll fields from those updates. So toll is “fetched” from the Ride table only in those flows that load the ride, not inside the batch step.

**For SEPC you’d mirror this:**

- **Read** accumulated SEPC only from **Redis** in the batch/on-ride flow.
- **Write** that to the ride via something like `updateStateEntryPermitChargesAndNamesAndIds` in RideExtra (no need to read SEPC from the Ride table in that step).
- Any other code that needs current SEPC (e.g. end ride, APIs) would get it from the **Ride** entity they already load from DB, which was updated from Redis in the same way.

---

**User**

You said we are using Ride table, not redis for read?

---

**Cursor**

There are two different “read” paths; both are correct for their context.

**1. Inside the on-ride batch step (Internal.hs)**  
When we need the *current accumulated toll* to persist it:
- We **read from Redis** (OnRideTollCharges, OnRideTollNames, OnRideTollIds).
- We do **not** read toll from the Ride table there.
- We then **write** that value to the Ride table via `updateTollChargesAndNamesAndIds`.

So in the **location-updates flow** we use **Redis for read** (and Ride table only for write).

**2. Everywhere else (end ride, ride APIs, etc.)**  
When we need toll for fare, UI, or API response:
- We **load the Ride** from Postgres (e.g. `getActiveByDriverId`, `findById`).
- The Ride entity already has `tollCharges`, `tollNames`, `tollIds` from the last time the batch step (or ride end) ran `updateTollChargesAndNamesAndIds`.
- So we **read from the Ride table**; we don’t go to Redis for toll in these flows.

So:
- **Batch step:** read toll from **Redis**, write to **Ride table**.
- **End ride / APIs / any code that loads a ride:** read toll from **Ride table** (as part of the loaded Ride entity).

The “we read only from Redis” applies to the **batch step**. The “we use Ride table for read” applies to **end ride and other flows** that load the ride from the DB. No contradiction: different flows, different source of read.

---

**User**

@Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs @Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Lib/LocationUpdates.hs @Backend/lib/location-updates/src/Lib/LocationUpdates.hs 

Here for tolls we have batch of size 25 which is concatnated to form a bigger segement which is snapped to road on which we call toll calculation, correct?

---

**Cursor**

Checking batch sizes and how waypoints are accumulated and processed.


Checking how waypoints are added and whether 25 is used anywhere in this flow:


Here’s what the code does; it’s a bit different from “batch of size 25 concatenated”.

**What the code actually does**

1. **Two different “batch” numbers**
   - **`batchSize`** (first argument to `mkRideInterpolationHandler`): from config `normalRideBulkLocUpdateBatchSize`, default **98**. This is the **threshold**: we only run snap-to-road when `waypoints count > batchSize` (i.e. at least 99 waypoints when default is 98).
   - **`maxSnapToRoadReqPoints`**: hardcoded **98**. So we **read 99** waypoints per step: `getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)`.

2. **No “25” in this flow**
   - There is no 25 in the location-updates / toll flow. Defaults are 98 (and 99 for the segment size). So we do **not** have “batch of size 25” in this code; we have “trigger when > 98” and “process 99 at a time”.

3. **How the “bigger segment” is formed**
   - Waypoints are **appended** to a single Redis list (`addPoints` on each location update; each call can send any number of points).
   - When `waypoints count > batchSize`, we take the **first 99** from that list and:
     - snap them to road in **one** snap-to-road call,
     - run toll calculation on that **single** snapped segment (no concatenation of multiple snap results).
   - So the “bigger segment” is just “first 99 waypoints from the accumulated list”, then one snap, then toll on that. Nothing is explicitly “concatenated” from 25-sized batches; the list is whatever was pushed over time.

**Direct answers**

- **Batch size for when we run toll:** Trigger when waypoints > **batchSize** (default **98**). Segment size for snap + toll is **99** waypoints (maxSnapToRoadReqPoints + 1). So we do **not** have “batch of size 25” here.
- **Concatenation:** Waypoints are concatenated in the sense that they’re all pushed onto one Redis list; we then take the first 99 and pass that to snap-to-road, then to toll. We do **not** concatenate several already-snapped segments and then run toll on that; we run toll once per 99-point (raw) segment, on its **single** snapped result.

So: for tolls we use a **segment of 99 waypoints** (from the accumulated list), snap that once, then run toll on that snapped segment. The “batch of size 25” doesn’t appear in this flow; if your client sends 25 points per request, those just get appended until count > 98, then we process 99 at a time as above.

---

**User**

Does this logic for SEPC in Ride End Sound Right?

Ride End Flow:
We will these case if estimated SEPC ids = calculated SEPC ids then we are sure
When both are different
Distance Calculation failed
Neutral, use estimated charges
Distance calculation worked but still different (driver took different route)
Sure, use detected charges




---

**Cursor**

### Short answer

**Almost right, but I’d tweak the confidence + which charges you use in some edge cases.**

### A more precise version

Let:

- `estimatedIds` / `estimatedCharges` = from booking/quote.
- `detectedIds` / `detectedCharges` = from SEPC detector at end ride.
- `distanceFailed` = distanceCalculationFailed or too many self-tuned calls.
- `pickupDropOutsideThreshold` = rider clearly took a very different route.

#### 1. Estimated IDs == detected IDs

- **Condition:** `estimatedIds == detectedIds` (order-insensitive set compare), and tracking didn’t obviously fail.
- **Confidence:** **Sure**
- **Charges to use:** **detectedCharges** (or estimatedCharges; they should match or be very close).

This matches what you wrote.

#### 2. Different IDs, distance calculation failed

- **Condition:** `estimatedIds /= detectedIds` and `distanceFailed == True`.
- **Semantics:** Tracking is unreliable (snap-to-road / batches failed), detector result can’t be trusted.
- **Safer behavior:**
  - **Confidence:** **Unsure** or **Neutral** (your “Neutral, use estimated charges” is fine as long as product is OK with it).
  - **Charges to use:** Prefer **estimatedCharges** (because that’s the only stable thing you have).
  - If no estimate exists: I’d lean to **no SEPC** or a very conservative path, but that’s a product call.

So your “Neutral, use estimated charges” is reasonable, but you probably want to label it **Unsure** (like toll does) because it’s really “we charged based on estimate only”.

#### 3. Different IDs, distance is OK, driver took different route

Here you have two sub-cases:

- **3a. Route within threshold (pickup/drop not outside threshold)**  
  - That means rider roughly followed the expected corridor; mismatch is more likely due to detector or small routing differences.
  - **Conservative behavior (like toll):**
    - Allow **charging estimated IDs that were *not* detected** only when pickup/drop within threshold → **Neutral**.
    - Don’t charge any *extra* detected IDs that were never estimated (unless product explicitly wants that).

- **3b. Route deviated (pickupDropOutsideThreshold == True)**  
  - Rider took genuinely different route; detector’s `detectedIds` probably reflect reality better.
  - **Your proposal:** “Sure, use detected charges” makes sense here:
    - **Confidence:** **Sure** (or at least Neutral→Sure depending on how strict you want to be).
    - **Charges to use:** **detectedCharges**, ignoring estimate (estimate was for a different route).

So I’d refine your line to:

- **If distance calc worked AND pickup/drop outside threshold (route clearly changed):**
  - **Confidence:** **Sure**
  - **Charges:** **detectedCharges**

- **If distance calc worked AND pickup/drop within threshold but IDs differ:**
  - **Confidence:** **Neutral**
  - **Charges:** Typically **estimatedCharges** or **detected ∪ estimated but capped by estimate** (don’t bill IDs the quote never mentioned, unless product says OK).

### TL;DR

- **Equal IDs → Sure, detected.**
- **Tracking/distance failed → use estimate, mark Unsure/Neutral.**
- **Different IDs + route clearly deviated → Sure, use detected.**
- **Different IDs + route roughly as expected → Neutral, prefer estimate (and don’t charge unexpected extra IDs).**

If you tell me your final confidence enum and safety requirements (“never charge for SEPC IDs outside estimate” vs “allow extras”), I can tighten this into exact if/else cases.

---

**User**

Sure, can you just give me in short the updated case. Or just add to the plan.@.cursor/plans/state_entry_permit_charges_implementation_41fa27d8.plan.md 

---

**Cursor**

### SEPC end-ride logic (short version)

Let `estimated` = (IDs, amount) from booking, `detected` = from SEPC detector, and use set comparison on IDs:

- **1. IDs match (same set):**  
  - **Confidence:** Sure  
  - **Charges used:** Detected (or estimated; they should match)

- **2. IDs differ AND distance calc failed / tracking unreliable:**  
  - **Confidence:** Unsure (or Neutral, but mark as “estimate-based”)  
  - **Charges used:** Estimated

- **3. IDs differ AND distance calc OK:**
  - **3a. Route clearly deviated (pickup/drop outside threshold):**  
    - **Confidence:** Sure  
    - **Charges used:** Detected  
  - **3b. Route within threshold (route broadly as expected):**  
    - **Confidence:** Neutral  
    - **Charges used:** Prefer Estimated (don’t newly charge SEPC IDs never present in estimate unless product explicitly allows)

---

**User**

@.cursor/plans/state_entry_permit_charges_implementation_41fa27d8.plan.md Update this in plan clearly

---

**Cursor**

The plan is now updated with the clarified SEPC end-ride cases and confidence/charge rules.

---

**User**

Where does end ride get the estimated tolls? And where will it get the estimated sepc? Detected were in redis right?

---

**Cursor**

### Where end-ride gets **estimated** vs **detected** tolls / SEPC

#### 1. Estimated tolls – source

In `EndRide.hs`:

```12:15:Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Ride/EndRide.hs
let estimatedDistance = fromMaybe 0 booking.estimatedDistance
    estimatedTollCharges = rideOld.estimatedTollCharges
    estimatedTollNames   = rideOld.estimatedTollNames
    estimatedTollIds     = rideOld.estimatedTollIds
...
finalDistanceCalculation ... estimatedDistance estimatedTollCharges estimatedTollNames estimatedTollIds ...
```

- `rideOld.estimatedToll*` comes from **ride creation** (Confirm / `SharedLogic.Ride`), which copies them from **booking/search estimate**.  
- So **estimated tolls are from the DB Ride row (backed by booking/quote)**, not from Redis.

For SEPC, the plan is identical:

- Add `estimatedStateEntryPermitCharges/Names/Ids` on `Ride`, populated from `Booking` at Confirm.
- In `EndRide`, read `rideOld.estimatedStateEntryPermit*` and pass them into `finalDistanceCalculation` just like toll.

#### 2. Detected tolls – where they live

During the ride:

- Each on-ride batch (`recalcDistanceBatchStep` in `Internal.hs`) writes **detected tolls** into Redis:

  - `OnRideTollCharges:<driverId>` (sum),
  - `OnRideTollNames:<driverId>`,
  - `OnRideTollIds:<driverId>`.

- After each batch, we read those Redis keys and call `QRide.updateTollChargesAndNamesAndIds`, so **ride.tollCharges/Names/Ids in Postgres are kept in sync**.

At end ride:

- `finalDistanceCalculation` runs one last batch pass, again using Redis for accumulation, and then flushes to the Ride row via that same update.
- `EndRide.hs` then reloads `updRide` from DB and uses:

  - `updRide.tollCharges`, `updRide.tollNames`, `updRide.tollIds` as the **detected tolls**, and
  - calls `checkAndValidatePendingTolls` using those plus any **pending** entries stored separately in Redis (`TollGateTracking:DriverId-...`).

So:

- **During on-ride:** detected tolls live primarily in **Redis**, with periodic sync into the Ride row.
- **At end-ride decision time:** we use the **Ride row fields** (`updRide.toll*`) as “detected”, plus the pending-tolls Redis key for the entry-without-exit cases.

For SEPC in your plan:

- Detected SEPC will follow the same pattern:
  - **On-ride:** accumulate in SEPC-specific Redis keys, then sync into `ride.stateEntryPermitCharges/Names/Ids`.
  - **End-ride:** treat `updRide.stateEntryPermit*` as detected SEPC; `checkAndValidatePendingStateEntryPermits` compares those with `estimatedStateEntryPermit*`, without a separate “pending SEPC” Redis key.

---

**User**

We dont want to fetch DB for geom and bounding box for every city again and again. We will fetch all of them and store it in pod's cache in memory. And then use it from their. 

Like this:
"IM.withInMemCache ["CACHED_VEHICLE_SERVICE_SUBTYPES", vehicleNumber] 86400 $ do
   mbVehicleLiveRouteInfo <- getVehicleLiveRouteInfo integratedBPPConfigs vehicleNumber Nothing
   pure $ mbVehicleLiveRouteInfo >>= (\(_, v) -> v.serviceSubTypes)
"


When exactly should we fetch it? When should we clear it?

---

**Cursor**

### Short answer

- **Where to fetch (populate cache):**  
  Inside the SEPC detector entry point (e.g. `getStateEntryPermitInfoOnRoute`), on **first call per `merchantOperatingCityId`**, using `IM.withInMemCache`. That way:
  - First request for that city hits DB → builds all geoms + bounding boxes → caches them.
  - Subsequent SEPC calls for that city in the same pod reuse the in‑memory value.

- **When to clear / refresh:**
  - **Normal case:** Let `withInMemCache`’s TTL handle expiry (e.g. **24 hours**), so you don’t manually clear; after TTL, next call recomputes from DB.
  - **When geoms/config change more often or need immediate pickup:**  
    - Add a **version/epoch** (e.g. from a config table or SEPC schema version) into the cache key:  
      `["CACHED_SEPC_GEOMS", merchantOpCityId.getId, sepcVersion]`.  
      A version bump on migration/config change automatically “invalidates” old entries.
  - **On deploy:** you don’t need to clear explicitly; pod restart naturally clears in‑process cache.

### Concrete pattern

- **Key design:**

```haskell
IM.withInMemCache ["CACHED_SEPC_GEOMS", merchantOperatingCityId.getId] 86400 $ do
  -- One-time per city per TTL:
  -- 1. Fetch all SEPC rows for this city
  -- 2. Build in-memory geoms + bounding boxes
  pure sepcGeomCacheForCity
```

- **Usage:**
  - `getStateEntryPermitInfoOnRoute` calls this at the top, gets `sepcGeomCacheForCity`, and then runs all per-route logic purely in memory.

So: **fetch lazily on first SEPC call per city, keep it in pod memory via `withInMemCache`, rely on TTL (and optionally a versioned cache key) for refresh; no manual clear needed in normal flow.**

---

**User**

Merchant operating city we are dropping:

What do I store in SEPC table for state and city, in Geom table geometry is of State probably so we should just store Uttarpradesh, Delhi, Haryana right? No city and merchantOperaatingCityId is required?
Merchant operating city id is not required
We can drop city and state as well and just keep geom id as foreign key which has state and city as any city. So when we query we fill fetch where state = state and city = anycity

Now tell again when do we fetch and store

---

**Cursor**

### 1. What to store in the SEPC table

Given you’re **dropping merchantOperatingCity** and geometry is already in a **Geom** table:

- **SEPC table should not duplicate state/city**. Let Geom be the source of truth.
- A clean SEPC schema looks like:

- **Columns (core):**
  - `id` (SEPC id)
  - `geom_id` (FK → `geom.id`)
  - `charge` (money)
  - `name` (e.g. "UP Entry Permit")
  - any other SEPC-specific flags (vehicle type, validity, etc.)

- **Optional denormalized fields (for reporting only):**
  - `state_code` (e.g. “UP”, “DL”) – redundant but cheap
  - `country_code` if needed

You **don’t need**:
- `merchant_operating_city_id`
- separate `state` / `city` columns for lookup (those already live in `geom`).

When you need to filter by state/city, you join:

```sql
FROM sepc
JOIN geom ON sepc.geom_id = geom.id
WHERE geom.state = <state>
  AND (geom.city = <some city> OR geom.city = 'ANYCITY' OR whatever rule)
```

If your geoms are state-level (no per-city shape), you often don’t even need the city filter.

---

### 2. When to fetch and store in in‑pod cache

Now that SEPC is **not** per merchantOperatingCity, the cache can be global or per state. Two reasonable patterns:

#### Option A – Global SEPC geom cache (simplest)

- **When:** On **first SEPC detector call in the pod**, regardless of city/state.
- **How:**

```haskell
IM.withInMemCache ["CACHED_SEPC_GEOMS"] 86400 $ do
  -- One-time per pod per 24h:
  -- 1. SELECT all active SEPC rows JOIN their geoms
  -- 2. Build in-memory structures: [ (sepcId, charge, polygon, stateCode, ...) ]
  pure sepcGeomCache
```

- **Use:** Every call to `getStateEntryPermitInfoOnRoute`:
  - Reads the whole `sepcGeomCache` from memory,
  - Quickly filters by state / bounding box in memory,
  - Runs segment crossing logic.

- **Clear/refresh:**
  - TTL (e.g. 86400s) in `withInMemCache` handles expiry.
  - A pod restart also resets it.
  - If you add a SEPC “version” column or config, you can bake that into the key:
    - `["CACHED_SEPC_GEOMS", sepcVersion]` so bumping `sepcVersion` auto-invalidates old cache.

#### Option B – Per-state cache (if many states / large data)

If data size grows, you can lazily cache **per state**:

```haskell
IM.withInMemCache ["CACHED_SEPC_GEOMS", stateCode] 86400 $ do
  -- Load only SEPC + geoms for this state
  pure sepcGeomCacheForState
```

Then in the detector:

- Derive `stateCode` from the route (first/last point’s state).
- Fetch that state’s cache via `withInMemCache`.
- TTL + optional version still control refresh.

---

### 3. Direct answers

- **What do I store in SEPC table for state/city?**  
  - **Store only `geom_id`** (FK) plus SEPC fields (charge, name, etc.).  
  - Let **Geom** carry `state`/`city`. No `merchantOperatingCityId` needed.

- **Merchant operating city id is not required?**  
  - Correct, if SEPC is per physical state/geom, **drop MOC from SEPC**.

- **When do we fetch and store?**  
  - Inside the **SEPC detector entrypoint** (`getStateEntryPermitInfoOnRoute`), on **first call**:
    - Use `IM.withInMemCache` to load all SEPC+geom (or per state) from DB and store in pod memory.
  - Rely on **TTL (and optional version in key)** for refresh; no manual clear in normal flow.

---

**User**

Lets say i modify geom postgres table, then how do i update the in mem cache? for 24 hrs the changes will not reflect in worst case

---

**Cursor**

### Short answer

You’re right: with a plain 24h TTL, **worst case you’ll see stale geoms for 24h** after a DB change. To avoid that you need an **explicit invalidation signal**, not just time.

### Practical strategies

Pick one (or combine 1 + 3):

- **1. Versioned cache key (recommended)**
  - Add a small `sepcGeomVersion` (or generic `geomVersion`) somewhere central:
    - e.g. a row in a config table, or a value in Redis / Dhall config.
  - Cache with that version in the key:

    ```haskell
    geomVersion <- getGeomVersion  -- from config/Redis/DB
    IM.withInMemCache ["CACHED_SEPC_GEOMS", geomVersion] 86400 $ do
      -- load all current geoms + SEPC
      pure geomCache
    ```

  - **When you run a geom migration or edit geoms**, part of the deployment / migration script:
    - **bump `geomVersion`** (e.g. from 42 → 43).
  - All pods will, on next request:
    - See a new key (`["CACHED_SEPC_GEOMS","43"]`), miss the cache, and reload from DB.
  - Old entries for `"42"` just expire or sit unused.

- **2. Shorter TTL + “soft” staleness**
  - If you can tolerate some delay and avoid plumbing a version:
    - Use a **shorter TTL** (e.g. 5–30 minutes).
    - Worst‑case staleness is that TTL, not 24h.
  - Still no hard guarantee, but much better.

- **3. Explicit “clear cache” trigger**
  - Add an **admin-only endpoint / background task** that:
    - Either calls a `IM.clearInMemCache`-style helper (if available), or
    - Bumps a version in Redis/DB (same idea as 1, but triggered manually).
  - After you modify the geom table:
    - Call this endpoint once to invalidate all pods.

- **4. Hybrid**
  - Use **versioned key + reasonable TTL**:
    - Normal operation: version changes on schema/content updates.
    - Safety: if someone forgets to bump the version, TTL eventually refreshes anyway.

### Concrete recommendation for your SEPC/geom case

- Introduce a **`geomConfigVersion`** (or `sepcConfigVersion`) integer in a small config table.
- Use:

  ```haskell
  version <- getGeomConfigVersion
  IM.withInMemCache ["CACHED_SEPC_GEOMS", show version] 86400 $ ...
  ```

- In any migration or manual change to `geom`/SEPC shapes:
  - Add a tiny step: `UPDATE config SET geom_config_version = geom_config_version + 1;`
- This gives you **instant-ish** propagation on next request, without waiting 24h, and without needing to know about individual pods.

---

**User**

how about we add version in the key name, whenever we update geom we also release a change with updating that version like geom:v2 to geom:v3, does that sound like a good idea? We can keep ttl as well along with it for 12 hrs

---

**Cursor**

Yes, that’s exactly the right pattern—and it’s a good idea.

### How it would look

- **Key format:** e.g. `["CACHED_SEPC_GEOMS", "geom:v3"]`
- **On read:**

```haskell
let geomVersion = "geom:v3"  -- from config / code / env
IM.withInMemCache ["CACHED_SEPC_GEOMS", geomVersion] (12 * 60 * 60) $ do
  -- Hit DB once for this version:
  -- 1. Load all SEPC + geoms
  -- 2. Build in-memory structures
  pure sepcGeomCache
```

- **On geom change:**
  - You bump the version in code/config from `"geom:v3"` → `"geom:v4"`.
  - Next request in each pod:
    - Misses `"geom:v4"` in in-mem cache,
    - Rebuilds from DB once,
    - Old `"geom:v3"` entries naturally die or sit unused until TTL.

### Why this works well

- **Instant invalidation:** As soon as new code with `"geom:v4"` is deployed, all pods start using fresh data on first request—no 12h wait.
- **Per-pod, no coordination:** You don’t need to call any “clear cache” RPC; version bump is the invalidation.
- **TTL still useful:** 12h TTL is a safety net (e.g. if you forget to bump version, it eventually refreshes anyway).

Just make sure:

- The **version value comes from one place** (config/Dhall/DB/env), not hardcoded in multiple modules.
- All geom/SEPC cache users use the same `"geom:vX"` so you don’t accidentally have multiple independent caches.

---

**User**

Add this to plan@.cursor/plans/state_entry_permit_charges_implementation_41fa27d8.plan.md 

---

**Cursor**

Added a concise section to the plan describing the versioned in-pod geom cache (e.g. `["CACHED_SEPC_GEOMS","geom:v3"]` with 12h TTL and version bumps on geom changes).

---

**User**

"**Geometry / point-in-polygon:** Use existing `findGeometriesContaining` / `findGeometriesContainingGps` only where cheap (e.g. after narrowing candidate geoms by state). Prefer: fetch candidate geoms once by geom_id, then in-memory point-in-polygon for each route point to avoid per-point DB cost. If no in-memory geom lib exists, use one bulk DB query that returns which geom contains each route point. "

Whats this we did not discuss it

---

**Cursor**

That paragraph is just an implementation hint that predates our last few messages; we didn’t design it together in this conversation.

In plain terms it’s saying:

- **Don’t** call `findGeometriesContaining*` per point/segment against the DB; that would be too expensive.
- **Do** either:
  - Load the relevant geoms **once** (what we are now planning to do via the in‑pod cache) and run **point‑in‑polygon in memory**, or
  - If you don’t have an in‑memory polygon lib, do a **single bulk DB query** that tells you which geom contains which route points, instead of N point lookups.

So conceptually it’s aligned with what we discussed (minimize DB hits by doing geom work in memory), but the specific references to `findGeometriesContaining` / “bulk DB query per point” are just legacy wording in the plan, not a hard requirement. You can safely interpret that section as: “optimize SEPC by loading geoms once (from cache) and doing the geometric checks in memory, rather than DB per point.”

---

**User**

"If no in-memory geom lib exists, use one bulk DB query that returns which geom contains each route point.  "?

---

**Cursor**

That sentence was describing an **alternative** (fallback) optimization if you *don’t* implement an in‑memory polygon check.

- The idea: instead of doing `N` separate DB calls like “which geom contains point pᵢ?”, send **one query** to Postgres with **all route points** and let PostGIS do `ST_Contains` in bulk, returning something like `(point_index, geom_id)` for every point‑geom containment.
- Then your Haskell code just works over that result set, without repeated round‑trips.

In your current plan, though, we’re **not** relying on that fallback: we’re going with “load all SEPC geoms once into pod memory (via the versioned `withInMemCache`) and run point‑in‑polygon in memory.” So you can treat that “bulk DB query” line as a low‑priority alternative, not something you must implement.

---

**User**

1. Remove it because we are going with implement an in‑memory polygon check

2. Also add that bounding box will be store in geometry table as well correspinding to each state and city as anycity.

3. Also update the table structure of SEPC accordingly


Update plan with this changes@.cursor/plans/state_entry_permit_charges_implementation_41fa27d8.plan.md 

---

**Cursor**

Updated the plan to (1) remove the bulk-DB fallback sentence, (2) state that bounding boxes are stored in the Geometry table per state/`ANYCITY`, and (3) simplify the SEPC table to use `geom_id` (no city/state/MOC) with updated query/migration notes.

---

**User**

"Merchant_state / allowed_states: use [Domain.Types.MerchantState](Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Domain/Types/MerchantState.hs) (`state`, `allowedDestinationStates`); filter SEPC rows by joining via `geom_id` to Geometry and then applying `state ∈ allowedDestinationStates` (and any city = ANYCITY rule, if used)."

explain this

---

**Cursor**

### What that sentence is saying

Break it down:

- **`MerchantState` rows define routing/SEPC policy per source state.**

```12:17:Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Domain/Types/MerchantState.hs
data MerchantState = MerchantState
  { allowedDestinationStates :: [IndianState]  -- where rides/traffic are allowed to go
  , merchantId               :: Id Merchant
  , state                    :: IndianState    -- the "source" state
  , ...
  }
```

So for a merchant you might have, for example:

- `state = KA`, `allowedDestinationStates = [KA, TN, AP]`
- `state = DL`, `allowedDestinationStates = [DL, HR, UP]`

This answers: “from this **source** state, which **destination** states are allowed?”

---

### How this ties into SEPC + geometry

Your SEPC table now has:

- `geom_id` → FK into `Geometry`
- Geometry has `state`, `city` (often `ANYCITY`), `geom`, `bounding_box`, etc.

So to pick **candidate SEPC rows** for a ride:

1. **Determine source state** for the ride (e.g. from pickup geom).
2. **Look up `MerchantState`** for that merchant + source state, to get `allowedDestinationStates`.
3. **Query SEPC via geometry:**

   - Join `state_entry_permit_charges` → `geometry` on `geom_id`.
   - Filter `geometry.state ∈ allowedDestinationStates`.
   - Optionally filter `geometry.city = 'ANYCITY'` (if your convention is “SEPC applies to the whole state regardless of city”).

In words:

- “Use `MerchantState` to get the **set of allowed destination states** for this source state, then only consider SEPC entries whose **geometry’s state** is in that allowed set (and, if you use it, whose city is `ANYCITY`).”

---

**User**

Some doubts earlier which i had has answers now, please check and update plan with these answers:

How to create a bounding box for testing in geojson.io? What is the query to create GEOM corresponding to geojson generated on geojson.io by connecting 4 points?
Should bounding boxes be an array of few points or should I store it as geom in the geometry table?
JSON of 4 points
How to generate geom?
We don't have to generate GEOM, we can use exiting geom.


Any existing code to get 4 sides of bounding box or give line segment corresponding to start and end lat-long? We can simply store it as array treat consecutive points as lines.
Keep it as a JSON of 4 points like toplegt, topright, bottom left, bottom right


For intercity rides there is no allowed states, how do i know which states bounding box i need to consider?
The merchant state table needs to be there for intercity rides as well. We will check this table.

This is already updated in plan i guess:
What do I store in SEPC table for state and city, in Geom table geometry is of State probably so we should just store Uttarpradesh, Delhi, Haryana right? No city and merchantOperaatingCityId is required?
Merchant operating city id is not required
We can drop city and state as well and just keep geom id as foreign key which has state and city as any city. So when we query we fill fetch where state = state and city = anycity


How does redis key only driver id has only driver id for 6h ttl, what if driver takes a trip for more than 6 hours how will we differentiate those 2 trips?
On ride end key is cleared



I think we should only count unique entries in a state, zig zag case should be handled by checking if the user has not entered this state before on this ride.
Yes just keep unique



@.cursor/plans/state_entry_permit_charges_implementation_41fa27d8.plan.md 

---

**Cursor**

Here’s how I incorporated those answers into the plan:

- **Bounding boxes & geom:**
  - Clarified that we **store bounding boxes in the Geometry table** as a JSON/array of **4 points** (`topLeft`, `topRight`, `bottomRight`, `bottomLeft`), treating consecutive points as sides.
  - Noted that for testing you can draw rectangles in geojson.io and copy the 4 corners into this JSON, but we **reuse existing `geom` rows** (no need to generate new geoms from the box).

- **SEPC table structure:**
  - Updated `StateEntryPermitCharges` to have `id`, `amount`, `geom_id` (FK), optional `name`/flags, and **no city/state/MOC columns**; state/city come from the `Geometry` row via `geom_id`.

- **MerchantState / allowed states (including intercity):**
  - Kept the guidance that we use `MerchantState.state` + `allowedDestinationStates` and join SEPC via `geom_id`→`Geometry` to filter by `geometry.state` (and `ANYCITY` if used), which covers both intra-city and intercity, assuming `MerchantState` is populated.

- **Redis TTL vs ride length:**
  - Expanded the cleanup note to say: SEPC keys are **explicitly cleared on ride end**, and the 6h TTL is just a safety net; keys are not reused across rides even for very long trips.

- **Zig-zag / unique state charging:**
  - Updated the SEPC segment loop’s **Charge rule** to say: charge when we cross into a SEPC state **only the first time per ride**, by maintaining a per-ride set of “already charged” SEPC state IDs and skipping subsequent zig‑zag re-entries into the same state.

---

**User**

The on ride flow i still not very confident about:

1. We need to process batch of approx 100 after calling snapToRoad (same as toll)
2. To concatinate 2 batches we can use existing reids key: prevBatchTwoEndPoints

Can you first explain you approach for on ride function for sepc and then we can update the plan when i agree

---

**Cursor**

### High-level approach for on-ride SEPC (what I’m proposing)

I’ll describe it in terms of the **existing toll flow**, pointing out exactly where SEPC plugs in and how batching + continuity work.

---

### 1. Where SEPC runs in the on-ride flow

- Today, for toll:
  - `recalcDistanceBatchStep` builds a **batch of ~100 raw points** (99 actually: `maxSnapToRoadReqPoints + 1`).
  - `interpolatePointsAndCalculateDistanceAndTollImplementation`:
    - Calls **snapToRoad** once for that batch.
    - Gets back **`response.snappedPoints`**.
    - Calls **`getTollInfoOnTheRoute (Just driverId) response.snappedPoints`**.

- For **SEPC**, we do **the same thing, at the same place**:
  - Use the **same snappedPoints** that toll uses for that batch.
  - Call something like `getStateEntryPermitInfoOnRoute (Just driverId) sepcRouteSnappedPoints`.

So: **one snap per ~100 raw points**, toll and SEPC both see that same snapped polyline (possibly extended by a stitch point, see below).

---

### 2. Batch size and processing (same as toll)

- **Batch building:**
  - Redis list `waypoints:<driverId>` holds all raw GPS points appended over time.
  - For a snap batch:
    - Read **99 raw waypoints**: `getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)`.
    - Call `snapToRoad` on those 99 → gives `snappedPoints` (length can differ, that’s fine).
  - After processing:
    - `deleteFirstNwaypoints driverId 98` → we keep 1 overlapping raw point for the next batch (raw continuity).

- **So point (1) from you is correct:**
  - We process **batches of ~100 raw points (99)**.
  - We call **snapToRoad once per batch**, then run toll + SEPC on the snapped route for that batch.

---

### 3. Continuity between batches (where we need to be careful)

The important nuance:

- Raw waypoints are continuous with **one-point overlap** (p98 is in both batches), but
- The **snapped** routes can be discontinuous across batches (S98 vs S98' for the same raw p98).

For SEPC we want to see **every segment of the snapped path**, including the one that crosses between “last snapped point of previous batch” and “first snapped point of current batch”.

**My stitched approach for SEPC:**

For each driver, we keep **one extra snapped point** in Redis:

- `LastSnappedPointForTollSepc:<driverId>` (or similar).

On each snap batch:

1. **Before SEPC call:**
   - After snapToRoad, we have `snappedPoints :: [LatLong]` for the current batch.
   - Read `mbPrevSnappedLastPoint` from Redis.
   - Build the SEPC route to analyze:

     - If `mbPrevSnappedLastPoint` is `Nothing` (first batch in ride):
       - `sepcRoute = snappedPoints`.
     - If `Just prevLast`:
       - `sepcRoute = prevLast : snappedPoints`.

   - Run:
     - `getTollInfoOnTheRoute (Just driverId) snappedPoints` (toll already handles inter-batch via its own pending key).
     - `getStateEntryPermitInfoOnRoute (Just driverId) sepcRoute` (SEPC sees a **continuous snapped route** across batches).

2. **After SEPC call (for next batch):**
   - Let `newLast = last snappedPoints` (if non-empty).
   - Store `newLast` back into `LastSnappedPointForTollSepc:<driverId>`.

3. **Cleanup:**
   - On ride end (or when clearing on-ride keys), delete `LastSnappedPointForTollSepc:<driverId>` along with SEPC Redis keys.

**Effect:**

- Toll logic is unchanged (it uses its own `TollGateTracking` state).
- SEPC detector always sees a **stitched snapped polyline**:
  - First call: `[S0..S98]`.
  - Second call: `[S98 (from prev), S98', S99', ...]`, so the boundary segment between batches is checked.

---

### 4. Why not use `prevBatchTwoEndPoints` directly for SEPC stitching?

`prevBatchTwoEndPoints` in `recalcDistanceBatches` is:

- A **raw** waypoint cache (last two points from the previous **location-update request**, not necessarily previous snap batch).
- Used only to:
  - Build `modifiedWaypoints = prevTwo <> currentWaypoints` to compute deviation (`updateRouteDeviation`).
  - It never feeds directly into snapToRoad or toll/SEPC detectors.

Problems if we try to reuse it for SEPC:

- It’s in **raw GPS space**, while SEPC detection needs a **continuous snapped route**.
- It’s at the **per-request** granularity, not exactly aligned with each 99-point snap batch.
- We’d still need to snap those previous raw points again to get “previous snapped last point” – effectively recreating what the dedicated `LastSnappedPointForTollSepc` key gives us more cleanly.

So my suggestion is:

- **Keep `prevBatchTwoEndPoints` as-is** for route deviation logic.
- **Use a separate per-driver key for the last snapped point** to stitch batches for SEPC (and, if desired, also pass the same stitched route to toll in future to close its minor gap).

---

### 5. SEPC accumulation & Redis (mirroring toll)

Per batch, after SEPC detector runs on `sepcRoute`:

- It returns something like `(sepcChargesBatch, sepcNamesBatch, sepcIdsBatch)`.
- We do the same pattern as toll:

  - Append names/ids to:
    - `OnRideStateEntryPermitNames:<driverId>`
    - `OnRideStateEntryPermitIds:<driverId>`
  - Increment charges in:
    - `OnRideStateEntryPermitCharges:<driverId>`

- Then (like toll) we periodically:
  - Read those SEPC keys in `recalcDistanceBatchStep`.
  - Call `updateStateEntryPermitChargesAndNamesAndIds driverId ...` to persist to the Ride row.

- **Redis TTL (e.g. 6h)** is just a safety net:
  - Keys are **explicitly cleared on ride end**, so they don’t leak across rides even if a driver drives longer than TTL.

---

### Summary of the proposed on-ride SEPC flow

1. **Batching:** Keep existing toll batching: snapToRoad on batches of 99 raw waypoints.
2. **Snapping:** Use the same `snapToRoad` call; SEPC sees the same `response.snappedPoints` as toll.
3. **Stitching across batches (for SEPC):**
   - Maintain a per-driver **last snapped point** key.
   - Prepend it to the current batch’s snapped points before calling SEPC, so SEPC sees a continuous snapped route across batches.
4. **Accumulation:** Mirror toll’s Redis pattern for SEPC (charges, names, ids).
5. **Persistence:** After each batch, read SEPC Redis keys and update the Ride row; at end ride, use the Ride’s detected SEPC fields + estimate to compute final `(charges, confidence)`.

If you want, next step I can translate this into a very small pseudo-code snippet matching your existing `recalcDistanceBatchStep` / `interpolatePointsAndCalculateDistanceAndTollImplementation` signatures so you can see exactly where each SEPC step slots in.

---

**User**

LastSnappedPointForTollSepc:<driverId>, we are already doing this check: prevBatchTwoEndPoints

---

**Cursor**

They’re related but not actually doing the same thing.

### What `prevBatchTwoEndPoints` does today

In `recalcDistanceBatches`:

```185:193:Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs
prevBatchTwoEndPoints :: Maybe [LatLong] <- Redis.safeGet $ lastTwoOnRidePointsRedisKey driverId
let modifiedWaypoints =
      case prevBatchTwoEndPoints of
        Just points -> NE.fromList points <> waypoints
        _ -> waypoints
...
(routeDeviation, tollRouteDeviation, isTollPresentOnCurrentRoute) <- updateRouteDeviation driverId (toList modifiedWaypoints)
...
Redis.setExp (lastTwoOnRidePointsRedisKey driverId) currentLastTwoPoints 21600
```

- It stores the **last two raw GPS points per location-update request**, not per snap batch.
- It is used **only for deviation detection** (`updateRouteDeviation`), never passed into `snapToRoad` or toll/SEPC detectors.
- Snap batches are built later from **`waypoints:<driverId>` in Redis**, without using `prevBatchTwoEndPoints` at all:

```273:283:Backend/lib/location-updates/src/Lib/LocationUpdates/Internal.hs
batchWaypoints <- getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)
(distance, interpolatedWps, ..., mbTollChargesAndNames) <-
  interpolatePointsAndCalculateDistanceAndToll distanceCalcReferencePoint' ... driverId batchWaypoints
```

So `prevBatchTwoEndPoints` ≠ “last snapped point of previous snap batch”; it’s “last two raw points from previous **request** for the sake of deviation”.

### Why `LastSnappedPointForTollSepc` is a different thing

For SEPC stitching we specifically need:

- The **last snapped point** from the **previous snap batch**, so we can prepend it to the **current batch’s snapped points** and run SEPC on a continuous snapped route.

`prevBatchTwoEndPoints` won’t give that because:

- It’s **raw** (pre-snap), not snapped.
- It’s per request, not aligned with the snap batches.
- It’s not wired into `interpolatePointsAndCalculateDistanceAndToll` at all.

We could, in theory, redesign the flow so snapToRoad also sees `modifiedWaypoints`, but with the current structure, a separate `LastSnappedPointForTollSepc:<driverId>` is the clean way to:

- capture exactly “last snapped point of last processed batch”, and
- use it only for stitching the SEPC (and optionally toll) route across snap batches.

---

**User**

Cool, then we can add this redis key. Can you update the plan for on-ride. Keep it detailed as you discussed, dont miss anything. Add the new redis key in the plan as well.

You are in agent mode

---

**Cursor**

### On-ride SEPC plan updates

- **New Redis key added** under Phase 3: `LastSnappedPointForTollSepc:<driverId>` to store the **last snapped point of the previous snap batch** (6h TTL, cleared explicitly at ride end).
- **On-ride flow (Phase 5.5) rewritten in detail**:
  - Keeps existing toll batching: snap-to-road on ~99 raw waypoints (`maxSnapToRoadReqPoints + 1`), one snap call per batch.
  - Extends the handler with `getStateEntryPermitInfoOnRoute` and `updateStateEntryPermitChargesAndNamesAndIds`.
  - For each batch:
    - Reads `LastSnappedPointForTollSepc:<driverId>`, builds `sepcRoute = prevLast : snappedPoints` (or just `snappedPoints` for the first batch).
    - Calls toll on `snappedPoints` as today, SEPC on `sepcRoute`.
    - Accumulates SEPC into `OnRideStateEntryPermitCharges/Names/Ids:<driverId>` and flushes to the Ride row via `updateStateEntryPermitChargesAndNamesAndIds`.
    - Updates `LastSnappedPointForTollSepc:<driverId>` with the last snapped point of the current batch.
    - Deletes 98 raw waypoints so the next batch overlaps by one raw point.
  - Cleanup section explicitly clears all SEPC OnRide keys **and** `LastSnappedPointForTollSepc:<driverId>` at ride end, with TTL noted as a safety net.

---

**User**

LastSnappedPointForTollSepc:<driverId>


Why the name has toll in it? We are adding feature for state entry permit charges

---

**Cursor**

You’re right, that suffix is misleading now.

- The original idea was “this key can be reused for toll and SEPC stitching,” hence `TollSepc`, but in practice **toll already has its own cross-batch handling** via `TollGateTracking:DriverId-…`, and the stitching is primarily for SEPC.
- For clarity and future readers, it’s better to name it **SEPC-specific** (or at least neutral), e.g.:
  - `LastSnappedPointForSepc:<driverId>`  
  - or more generic: `LastSnappedSnappingPoint:<driverId>` / `LastSnappedPointForOnRide:<driverId>` if you want to share it later.

I’d rename it in the plan (and code) to something like **`LastSnappedPointForSepc:<driverId>`**, so it’s obvious this key is about SEPC’s stitched snapped route, not toll.

---

**User**

"- **Overlap rule:** If either point is in more than one geom, skip this segment; move to next pair."


Why skip we need to select the state for second point in pair which is not equal to first point state. Does that make sense.

Lets say boundaries of 2 state overlap then we need to consider the state which is not equal to first state.

Update the plan with this change, also update the redis key name to LastSnappedPointOnRide

---

**Cursor**

- **Redis key rename:** Updated the plan so the stitching key is now called **`LastSnappedPointOnRide:<driverId>`** everywhere (table of keys and the detailed on-ride flow), since it’s a generic on-ride snapped-point cache, not toll-specific.
- **Overlap handling:** Changed the overlap logic to no longer blindly skip segments where a point lies in multiple geoms; now the plan explicitly says to charge using the **second point’s state that is different from the first point’s state** (`areaSecond \ areaFirst`), while still keeping the “charge only once per state per ride” rule for zig-zagging.

---

**User**

- **Overlap rule:** If either point is in more than one geom, skip this segment; move to next pair.

Remove this because we have added overlap case in this plan later

---

**Cursor**

Done. The separate “Overlap rule: skip this segment” bullet is removed; the plan now only has the updated **Charge rule** that already describes how to handle overlaps (choose `areaSecond \ areaFirst` and still apply the “first time per state per ride” logic).

---

**User**

@state_entry_permit_charges_implementation_41fa27d8.plan.md (206-274) 


Czn you check we are doing similar to tolls, not exactly but the flow is simialr?

"@state_entry_permit_charges_implementation_41fa27d8.plan.md (271-272) "


Also whats this above


Also we are not snapping separately for tolls and sepc right? We dont want to increase api calls for snapping. Are we resuisng them

---

**Cursor**

### 1. Is the SEPC on-ride flow “similar to tolls”?

Yes, it’s intentionally **very close** to toll’s flow, with two main differences:

- **Same pieces as toll:**
  - Use `processWaypoints` → `recalcDistanceBatches` → `recalcDistanceBatchStep`.
  - Build a raw batch of ~99 waypoints with `getFirstNwaypoints driverId (maxSnapToRoadReqPoints + 1)`.
  - Call **snapToRoad once** for that batch.
  - For toll: call `getTollInfoOnTheRoute (Just driverId) snappedPoints`.
  - Accumulate into OnRide Redis keys and then persist to the Ride row.

- **SEPC additions:**
  - Extra handler functions: `getStateEntryPermitInfoOnRoute` and `updateStateEntryPermitChargesAndNamesAndIds`.
  - One extra Redis key: `LastSnappedPointOnRide:<driverId>` to stitch batches for SEPC.
  - SEPC runs on `sepcRoute` which is `prevLastSnapped : snappedPoints` for continuity across batches.
  - Accumulation into SEPC-specific OnRide keys and persistence to Ride, mirroring toll.

So it’s “same pipeline, same snap and batching,” with SEPC-specific logic layered on top.

---

### 2. What is this part at 271–272?

```text
7. Advance raw waypoint window:
  - `deleteFirstNwaypoints driverId maxSnapToRoadReqPoints` (keep one raw overlap)
```

This is describing the **existing toll/location-updates behavior**, which SEPC shares:

- The raw waypoints are stored in a Redis list `waypoints:<driverId>`.
- For a batch we read 99 points: indices 0..98.
- After processing the batch, we call:

  ```haskell
  deleteFirstNwaypoints driverId maxSnapToRoadReqPoints  -- 98
  ```

- That trims the list so it now starts at the old index 98:

  - Before: `[p0, p1, ..., p97, p98, p99, ...]`
  - After deleteFirstNwaypoints 98: `[p98, p99, ...]`

- So the **next batch** reads `[p98..]`, i.e. raw batches **overlap by one point**.  
  That’s the “keep one raw overlap” comment.

SEPC doesn’t change this behavior; it just reuses it.

---

### 3. Are we snapping separately for toll and SEPC?

No, **snapToRoad is called only once per batch** and its result is reused:

- Step 2 in the “Exact logic” section:

  ```text
  2. Call `interpolatePointsAndCalculateDistanceAndToll` which:
    - Snap-to-road: `response.snappedPoints = snappedPoints`.
    - Toll: `mbTollInfo <- getTollInfoOnTheRoute (Just driverId) snappedPoints`.
  ```

- Then for SEPC:

  ```text
  3. For SEPC stitching:
     - mbPrevLastSnapped <- Redis.safeGet (LastSnappedPointOnRide:<driverId>)
     - sepcRoute = maybe snappedPoints (: snappedPoints) mbPrevLastSnapped
     - mbSepcInfo <- getStateEntryPermitInfoOnRoute (Just driverId) sepcRoute
  ```

So:

- **One snapToRoad call per batch** → `snappedPoints`.
- **Toll** uses `snappedPoints` directly.
- **SEPC** uses the **same `snappedPoints`**, optionally **prepended with one stored snapped point** from the previous batch to get `sepcRoute`.

No extra snap API calls are introduced for SEPC; we’re just doing additional processing on the already-snapped points.

---

**User**

"  deleteFirstNwaypoints driverId maxSnapToRoadReqPoints  -- 98"


this must be alreacy being done we dont need to do it again since we areworking on same points.

Also in our case concatination happens thorugh 

---

**Cursor**

- **`deleteFirstNwaypoints`**: Correct, that call is **already in the existing toll/location-updates flow** inside `recalcDistanceBatchStep`. In the SEPC plan we’re just *describing* that step so the batching story is complete; you do **not** add another delete. SEPC reuses the same batched raw waypoints and the same `deleteFirstNwaypoints driverId maxSnapToRoadReqPoints` that’s already there.

- **Concatenation in our case**: For SEPC, concatenation of batches happens **only at the snapped level** via `LastSnappedPointOnRide:<driverId>`:
  - Raw overlap: unchanged, still via `deleteFirstNwaypoints` (p98 appears in both raw batches).
  - SEPC route stitching: `sepcRoute = maybe snappedPoints (: snappedPoints) mbPrevLastSnapped`, i.e. we prepend the previous batch’s **last snapped point** to the current batch’s `snappedPoints`.  
So we’re not changing raw batching or adding deletes; we’re only adding this snapped-level prepend for SEPC.

---

**User**

Great can you add this few questions I asked in short to plan as well so we have that stored. Keep it short

---

**Cursor**

- The plan now has a short **FAQ / design clarifications** section (Phase 6) capturing your key questions and answers:
  - SEPC and toll **share the same snapToRoad call** per batch; SEPC reuses the snapped points (with stitching), so no extra snap API calls.
  - `deleteFirstNwaypoints` is **already handled by location-updates**; SEPC doesn’t add another delete and uses the same one-point-overlap batching.
  - The on-ride SEPC flow is **structurally the same as toll’s** pipeline, with SEPC-specific detector, Redis keys, stitched snapped route via `LastSnappedPointOnRide`, and its own confidence rules at end ride.

---

**User**

Can you start implementing the step 1 in my current branch on my local?

/Users/himanshukm-yatri/files/nammayatri                  ed737ab398 [backend/feature/sepc]

---

**Cursor**

I’ll first read Step 1 from the plan in your current worktree, then locate where “static toll” is coming from (FarePolicy/FareParameters/buildFare) and implement the change via the YAML specs + regeneration (no edits in `src-read-only`). After that I’ll compile the affected backend packages to ensure it builds.

I’m going to implement **Phase 1** from the plan: remove `FarePolicy.tollCharges` (schema + generated types/queries), stop adding it in fare calculation (use only dynamic toll from fare params), update any remaining reads (SearchTry/Dashboard), and add a migration to drop the column. Then I’ll run a `cabal build` for the affected provider package to verify it compiles.

---

**User**

continue but not in the wortree you created, i have deleted it and moved changes to my local using patch. Continue working there.

---

**Cursor**


Continuing Phase 1 in the main repo at `/Users/himanshukm-yatri/files/nammayatri`. Checking the repo state and remaining Phase 1 work.



Checking the main repo for remaining Phase 1 work:



Checking the rest of Queries/FarePolicy.hs and Merchant.hs for tollCharges and FullFarePolicy construction:



Searching Merchant.hs for tollCharges and FarePolicy construction; listing migrations.



Locating the FarePolicyCSVRow definition and where we build FarePolicy from CSV and set toll in the export:



Searching for FarePolicy construction from the CSV row and for mkUpdatedFarePolicy:



Finding where the FarePolicy record is built in makeFarePolicy:



Checking Storage for FullFarePolicy construction and locating migrations:



Applying Phase 1 changes: removing the CSV import's use of `tollCharges` and adding the migration.


