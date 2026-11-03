# Incentive Journey / Milestone — Design Doc

**Audience:** Engineers onboarding to Incentive Journey (driver primary; rider progress tracking)  
**Package:** `Backend/lib/incentive-journey`  
**Schemas:** Driver `atlas_driver_offer_bpp`; Rider `atlas_app` (dual-schema Beam)  
**Actors:** `DriverActor` (`driver-offer`), `RiderActor` (`rider-app`) — cached-query and eval-idempotency Redis keys are actor-prefixed (cash payout lock is a shared exception; see §13)

Related: `04-driver-app.md`, `06-ride-flow.md`, `09-dashboards.md`, `11-libraries.md`

---

## 1. Purpose

Configurable **multi-milestone campaigns**:

1. Ops defines a **Journey** (`Daily` / `Weekly` / `Monthly`) and ordered **Milestones** (condition + filters + reward).
2. Ops binds journey to a **Cohort** via **CohortJourneyMapping** (city campaign window + optional streak-end reward).
3. Users are **assigned** (manual UCM, bulk CSV/S3 on provider, or auto-apply by city/vehicle on provider).
4. On **ride complete**, the engine evaluates progress, awards rewards, and may pay a **streak-end** bonus.

**Driver maturity:** full eval + Coins/Cash/SWO + overlays + auto-apply + bulk S3.  
**Rider maturity:** eval + progress + dashboard; rewards mostly stubbed (`NoReward` real; others skipped).

When a **driver** has active journey assignments on a **DynamicOffer** valid ride, **legacy Incentive# / EndRide coin events are skipped** (journey takes precedence).

---

## 2. Package layout

| Area | Path |
|------|------|
| Library root | `lib/incentive-journey/` |
| Storage YAML | `spec/Storage/*.yaml` |
| Provider dashboard API | `spec/IncentiveJourney/ProviderPlatform/API/IncentiveJourney.yaml` |
| Rider dashboard API | `spec/IncentiveJourney/RiderPlatform/API/IncentiveJourney.yaml` |
| Re-export | `src/Lib/IncentiveJourney.hs` |
| Engine | `Eval`, `Condition`, `Window`, `Period`, `Streak`, `StreakEnd`, `Idempotency`, `Reward`, `Assignment`, `Display`, `Convert` |
| Orchestration | `Domain/Action/Evaluate.hs` |
| Dashboard | `Domain/Action/Dashboard/{Core,Provider,Rider,ServiceHandle}.hs` |
| UI | `API/UI/IncentiveJourney.hs`, `Domain/Action/UI/IncentiveJourney.hs`, `Common/UI/IncentiveJourney.hs` |
| Cached queries | `Storage/CachedQueries/*` |
| Driver adapter | `dynamic-offer-driver-app/.../SharedLogic/IncentiveJourney.hs` |
| Rider adapter | `rider-app/.../SharedLogic/IncentiveJourney.hs` |
| Ride-end (driver) | `SharedLogic/RideEvents/DriverCoinsAndJourney.hs` ← Kafka `RIDE_EVENTS` |
| Bulk job | `SharedLogic/Allocator/Jobs/IncentiveJourney/BulkUserCohortMappingUpload.hs` |
| Beam orphans | both apps `Storage/Beam/IncentiveJourney.hs` |

---

## 3. Conceptual model

```text
CohortDetails (audience)
        │
        ▼
CohortJourneyMapping ────────── IncentiveJourney (Daily|Weekly|Monthly)
  (city + window +                  │
   streakEnd reward)                ▼
        │                    IncentiveJourneyMilestone[] (ordered)
        ├── UserCohortMapping (explicit: person ↔ CJM)
        └── AutoApplyCohortMapping (city/vehicle; provider)

Progress: IncentiveJourneyStats
  unique (personId, cohortJourneyMappingId, milestoneId, periodKey)

Bulk audit (provider): BulkUserCohortMappingRun
```

- **Journey** — template (name + period type). No merchant columns.
- **Milestone** — one ordered step (condition + filters + reward).
- **CohortJourneyMapping (CJM)** — live city campaign (start, streak length, streak-end payout).
- **UserCohortMapping (UCM)** — this person is on that CJM until `validTill`.
- **Stats** — progress for one milestone in one period bucket under one CJM.

---

## 4. Storage schemas

### 4.1 `incentive_journey`

| Field | Type | Notes |
|-------|------|--------|
| id | PK | |
| name, description | Text / Maybe Text | |
| journeyType | `Daily \| Weekly \| Monthly` | default Daily |
| createdAt, updatedAt | UTCTime | |

### 4.2 `incentive_journey_milestone`

| Field | Type | Notes |
|-------|------|--------|
| id | PK | |
| journeyId | FK + SecondaryKey | |
| name, description | Maybe Text | |
| order | Int | sequential eval order |
| conditionType | see enums | |
| conditionOperator | default `GTE` | |
| conditionValue | Int | target |
| areaType | Maybe | special-location filter |
| specialLocationIds | Maybe [Text] | |
| vehicleCategory, serviceTierType | Maybe | ride must match if set |
| rewardType | `MilestoneRewardType` | |
| rewardValue, rewardExpirationAt | Maybe Int | SWO: % and daysValidFor |
| rewardMetadata | Maybe JSON | SWO: `{serviceName, waiveOffMode}` |
| timeBounds | Maybe TimeBound | peak window |

**Enums**

- `MilestoneConditionType`: `RideCompleted | Earnings | Distance | RideDuration | BookingTicket`
- `MilestoneConditionOperator`: `GTE | GT | EQ | LTE | LT`
- `MilestoneAreaType`: `Default | Pickup | Drop | PickupDrop`
- `MilestoneRewardType`: `Coins | Cash | Coupons | WalletMoney | SubscriptionWaiveOff | PoolingPriority | NoReward`

### 4.3 `cohort_details`

`id`, `name`, `category`, `description`, `cohortRule` (JSON). Unique `(category, name)`.

### 4.4 `cohort_journey_mapping`

| Field | Notes |
|-------|--------|
| merchantId, merchantOperatingCityId | city scope |
| cohortId, journeyId | unique with city |
| startDate, streakRange | window `[start, start + streakRange periods)` |
| enabled | |
| maxWaiveOffCount | default 1 |
| streakEndReward* | optional campaign completion bonus |

**Start-date rules** (`Streak.validateMappingStartDate`): Weekly → Monday; Monthly → 1st; Daily unrestricted.

### 4.5 `user_cohort_mapping`

`userId`, `cohortMappingId` (→ CJM), `enabled`, `validTill`. Unique `(userId, cohortMappingId)`. Active if `enabled && validTill >= now`.

### 4.6 `auto_apply_cohort_mapping` (provider-focused)

Auto-enroll by merchant/city/(optional) `vehicleCategory` (`null` = all categories).  
`allowIfNoMapping`: only when user has **no** active UCM. Both `row.enabled` and `cjm.enabled` required.

### 4.7 `incentive_journey_stats`

| Field | Notes |
|-------|--------|
| personId, merchant*, cohortId, journeyId, milestoneId | |
| cohortJourneyMappingId | Maybe (campaign instance) |
| periodKey | `Day:…` / `Week:…` / `Month:…` |
| condition* snapshot, currentValue | |
| status | see §5 |
| rewardType, rewardValue | |

Unique: `(personId, cohortJourneyMappingId, milestoneId, periodKey)`.

### 4.8 `bulk_user_cohort_mapping_run`

Status: `Scheduled | Running | Succeeded | Failed | Cancelled`.  
Tracks `s3FilePath`, offset, batchSize, inserted/skipped, `errorMessage`, scheduler job id.

---

## 5. Table inventory (complete)

These are **all** Incentive Journey–owned tables (YAML under `lib/incentive-journey/spec/Storage/`):

| # | Table | YAML | Role |
|---|-------|------|------|
| 1 | `incentive_journey` | `IncentiveJourney.yaml` | Journey template |
| 2 | `incentive_journey_milestone` | `IncentiveJourneyMilestone.yaml` | Ordered steps |
| 3 | `incentive_journey_stats` | `IncentiveJourneyStats.yaml` | Per-person progress |
| 4 | `cohort_details` | `CohortDetails.yaml` | Audience definition |
| 5 | `cohort_journey_mapping` | `CohortJourneyMapping.yaml` | City campaign + streak-end |
| 6 | `user_cohort_mapping` | `UserCohortMapping.yaml` | Explicit assignment |
| 7 | `auto_apply_cohort_mapping` | `AutoApplyCohortMapping.yaml` | City/vehicle auto enroll |
| 8 | `bulk_user_cohort_mapping_run` | `BulkUserCohortMappingRun.yaml` | Bulk CSV job audit |

No other Beam tables are defined in this library. Side effects touch **external** systems (§5.1).

### 5.1 External systems / tables touched at runtime (not owned by this lib)

| Concern | Where | Notes |
|---------|--------|--------|
| Overlay / PN templates | Overlay config by key | Keys below; city + language lookup |
| Driver coins | `Lib.DriverCoins` / coin ledger | `Coins.awardJourneyMilestoneCoins` |
| Cash payout | Payment / payout orders | Entity `INCENTIVE_JOURNEY_CASHBACK`; needs verified VPA |
| Subscription waive | Plan / subscription waive APIs | From SWO metadata |
| Special locations | Special-location master | UI name resolve; ride pickup/drop ids from booking area |
| S3 | `AWS.S3` | Bulk CSV store + job download |
| Allocator scheduler | `BulkUserCohortMappingUpload` job | Chunked UCM insert |
| Kafka ride events | `kafka-consumers` RideEnded | Driver eval trigger |
| Person / vehicle | Driver Person + Vehicle | Assignment resolution (city, category) |
| TransporterConfig / RiderConfig | City config | `timeDiffFromUtc` |

**Overlay keys** (`Display.hs`):

- `INCENTIVE_JOURNEY_MILESTONE_COMPLETED` — after successful milestone award (`onCompleted`)
- `INCENTIVE_JOURNEY_MILESTONE_WAIVED_OFF` — after dashboard waive

Templates support placeholders via `applyMilestoneOverlayTemplates` (journey name, milestone target text, reward amount, order).

---

## 6. Milestone status machine

```text
(no row) / NotStarted          ← UI may synthesize NotStarted when no stats row
       │  ride applies delta
       ▼
  InProgress ──condition met + Awarded──► Completed   (eval write path today)
       │                                    Rewarded   (enum kept; treated terminal /
       │                                               streak-satisfied; not written by
       │                                               current evaluate upsert)
       └──condition met + AwardSkipped──► RewardPending ──retry award──► Completed
                                              │
Dashboard waiveOff ──────────────────────────► WaivedOff
```

- **Terminal for sequencing:** `Completed | Rewarded | WaivedOff | RewardPending`
- **Streak-end satisfied:** same set
- **Eval persist mapping:** progress → `InProgress`; awaiting payout → `RewardPending`; success → `Completed` (+ `rewardValue`); waive → `WaivedOff`
- `RewardPending`: no more delta on that milestone; later rides retry award; after retry, ride **can** count on the next milestone

---

## 7. Ops setup flow (Control Center)

1. `POST /create` — Journey  
2. `POST /milestone/create` — ordered milestones  
3. `POST /cohort/create` — CohortDetails  
4. `POST /cohortJourney/create` — CJM (`startDate`, `streakRange`, optional streak-end)  
5. Assign:
   - `POST /assign` — single UCM  
   - `POST /assign/bulkFromS3` — CSV (provider)  
   - `POST /autoApplyCohort/create` — city/vehicle (provider)  
6. Ops: list / assignments / stats history / `POST /stats/waiveOff`

**Capability:** `system-config.coins.read` / `.write` (ApiAuthV2).

---

## 8. Assignment resolution

```haskell
data JourneyAssignment = JourneyAssignment
  { userCohortMapping :: Maybe UserCohortMapping  -- Nothing for pure auto-apply
  , cohortJourneyMapping :: CohortJourneyMapping
  , assignedAt :: UTCTime
  }
```

### Driver

App wrapper `SharedLogic.IncentiveJourney.findAssignmentsByUserId`:

- If **person exists** → `CQAssignment.findAssignmentsForDriver` (UCM **+** auto-apply).
- If **person missing** (`QPerson.findById` = `Nothing`) → UCM-only fallback (`findAssignmentsByUserId` in the lib). Auto-apply is **not** consulted.

`hasAssignedJourneys` = non-empty result of that wrapper. **When the person row exists**, auto-apply alone is enough to suppress legacy DynamicOffer coins and enter journey eval. If the person lookup fails, only UCM can make it true.

**Important (intentional gate, not window-aware):** suppression uses assignment existence only. `evaluateDriverJourney` then filters by `cjm.enabled` + active streak window. So an assignment **outside** the streak window (or only disabled CJMs after load) still **skips legacy Incentive# coins** and evaluates to a no-op. Journey membership takes precedence over Incentive# for the whole assignment lifetime (`validTill` / auto-apply), not only inside the streak. Do not tighten this to window-aware without an explicit product decision — that would re-enable legacy coins for out-of-window drivers and change current prod behavior.

`findAssignmentsForDriver` rules:

1. **UCM path:** load active UCMs (`enabled && validTill >= now`), join CJM by id. **No** merchant / city / vehicle re-filter on these rows — whatever CJM the UCM points at is returned (eval later still requires `cjm.enabled` + active window).  
2. **Auto-apply path only:** load enabled auto-apply for merchant/city/vehicle (`null` vehicle = all categories); require `row.enabled`, `cjm.enabled`, and CJM merchant/city match; `allowIfNoMapping` only if driver has **no** active UCM.  
3. Concatenate UCM + auto assignments; dedupe by `cohortJourneyMapping.id` (UCM wins uniqueness with auto for the same CJM).

### Rider

`CQAssignment.findAssignmentsByUserId` only (UCM). No auto-apply in rider SharedLogic today.

Lib eval (`Evaluate.hs`) always calls `findAssignmentsForDriver` (with ride vehicle category when present) — same UCM-vs-auto filter distinction as above.

---

## 9. Runtime evaluation

### 9.1 Triggers

**Driver (primary)**

```text
Kafka RIDE_EVENTS (RideEnded)
  → Processor.RideEvents.Handlers
  → processRideEndedCoinsAndJourney
       → fraud / daily coin block? → skip awards
       → DynamicOffer + valid ride:
            if hasAssignedJourneys:          -- UCM and/or auto-apply
              withJourneyEvalIdempotency(DriverActor, rideId)
                → evaluateDriverJourney
            else legacy DC.driverCoinsEvent
       → OTP + valid ride:
            if hasAssignedJourneys: suppress legacy coins only
            else legacy coins
            -- NO evaluateDriverJourney on OTP today
```

- Not on EndRide HTTP (latency isolation).  
- `timeBoundReferenceUtc` = `tripStartTime` (fallback `ride.createdAt`).  
- Pickup/drop special-location ids from `booking.area`.

**Rider**

```text
Domain.Action.Beckn.Common (ride completed)
  → fork "Evaluate rider incentive journey"
  → hasAssignedJourneys → withJourneyEvalIdempotency(RiderActor)
  → evaluateRiderJourney
```

Idempotency: Redis SETNX `{actor}:ride:processed:incentiveJourney:{rideId}` TTL **86400s**.

### 9.2 Pipeline (`evaluateIncentiveJourneys`)

1. `findAssignmentsForDriver` (± ride vehicle category).  
2. Local time = `timeBoundReferenceUtc + timeDiffFromUtc`.  
3. Load journeys by assignment ids; keep pairs where `cjm.enabled` and streak window active:  
   `startDate <= localTime < computeStreakEndDate(start, streakRange, journeyType)`.  
4. Per `(journey, cjm)`:  
   - `periodKey = mkJourneyPeriodKeyFor localTime journey`  
   - load milestones (ordered by `order`)  
   - build `MilestoneStore` (load/upsert stats via CQStats)  
   - `Eval.evaluateMilestonesInOrderWith`  
   - if streak-end configured → `tryAwardStreakEnd`

### 9.3 Ride deltas (`Condition.deltaForCondition`)

| ConditionType | Delta |
|---------------|--------|
| RideCompleted | `ridesDelta` (usually 1) |
| Earnings | fare (rounded) |
| Distance | chargeable meters |
| RideDuration | trip end − start (seconds) |
| BookingTicket | **always 0 on this path** (stub) |

### 9.4 Milestone filters (all must pass to apply delta)

Evaluated **before** adding delta; failure **stops the entire remaining chain** for this ride.

| Filter | Pass when |
|--------|-----------|
| `timeBounds` | `Nothing` / `Unbounded`, or local time inside an active peak (`Window.isWithinTimeBound`) |
| `vehicleCategory` | milestone unset, or equals ride category (shown Text) |
| `serviceTierType` | milestone unset, or equals ride service tier |
| `areaType` + `specialLocationIds` | see below |

**Location (`rideMatchesLocationFilter`):**

| areaType | Rule |
|----------|------|
| `Nothing` / `Default` | always pass |
| `Pickup` | ride pickup id ∈ `specialLocationIds` (missing list → **fail**) |
| `Drop` | ride drop id ∈ list (missing list → fail) |
| `PickupDrop` | **both** pickup and drop must match list |

Journey-level date window ≠ milestone peak: journey uses streak dates only; peaks are per-milestone.

### 9.5 Sequential eval rules (`Eval.hs`)

| State / filters | Behavior |
|-----------------|----------|
| Terminal status | Skip → **continue** next milestone |
| `RewardPending` or InProgress already meeting target | Retry award (**no delta**); then **continue** |
| Outside time/vehicle/location | **Stop chain** |
| Pass filters, condition not met | Persist `InProgress`; **stop** |
| Pass filters, condition met | Award → `Completed` or `RewardPending`; **stop** (no spill to next) |

Completing a milestone on a ride does **not** also count that ride on the next milestone.  
Retrying `RewardPending` **does** allow the ride to proceed to the next milestone after the retry.

### 9.6 App handle (rewards outside lib)

```haskell
data IncentiveJourneyHandle m = IncentiveJourneyHandle
  { actor :: JourneyActor
  , loadJourneysByIds :: ...
  , loadMilestones :: ...
  , dispatchReward :: Journey -> RewardDispatchCtx -> RewardSpec -> m AwardResult
  , dispatchStreakEndReward :: Journey -> campaignKey -> RewardSpec -> m AwardResult
  , mbOnMilestoneCompleted :: Maybe (Journey -> Milestone -> [Milestone] -> Int -> m ())
  }
```

Driver `mbOnMilestoneCompleted` → completed overlay. Waive path sends waived overlay from app dashboard wiring.

### 9.7 Reward dispatch matrix (driver)

| Kind | Behavior | On skip |
|------|----------|---------|
| `Coins` | `Coins.awardJourneyMilestoneCoins` | — |
| `Cash` | Payout order, entity `INCENTIVE_JOURNEY_CASHBACK`, lock `IncentiveJourneyCashLock:{referenceId}` | No verified VPA → `AwardSkipped` → `RewardPending` |
| `SubscriptionWaiveOff` | Plan waive from `%` / `daysValidFor` / `{serviceName, waiveOffMode}` | Invalid metadata → skip/log |
| `NoReward` | `Awarded 0` | — |
| `Coupons` / `WalletMoney` / `PoolingPriority` | Logged deferred | `AwardSkipped` |

**Cash retry:** after VPA verify (`Domain.Action.UI.Payout` / dashboard payout request) → `retryVerifiedVpaJourneyCashPayouts` backfills recent `RewardPending` cash rows.

**Streak-end:** same reward kinds via `dispatchStreakEndReward`; campaign key `campaign:{startDay}:{endDay}:{cjmId}` for idempotency/logging.

**Rider:** only `NoReward` awards; Coins/Cash refused; others stub-skipped.

### 9.8 Waive-off (`waiveMilestoneForPeriod`)

1. Validate milestone ∈ journey; CJM matches journey + city.  
2. Person must be assigned (current assignments **or** UCM row for CJM).  
3. Skip if stats already terminal.  
4. Enforce `maxWaiveOffCount` (count of `WaivedOff` rows for person+CJM+period).  
5. Upsert stats `WaivedOff` (currentValue at least conditionValue).  
6. Optionally `tryAwardStreakEnd`.

---

## 10. Period keys & streak

| JourneyType | periodKey | streakRange=2 exclusive end |
|-------------|-----------|-----------------------------|
| Daily | `Day:YYYY-MM-DD` | start + 2 calendar days |
| Weekly | `Week:YYYY-Www` | start + 14 days |
| Monthly | `Month:YYYY-MM` | start + 2 months |

Peak windows are **not** in the period key.

**Streak-end** (`StreakEnd.hs`), when `cjm.streakEndRewardType` set:

1. Cheap gate `shouldAttemptStreakEnd`: current period is **last** in streak **and** highest-`order` milestone satisfied for current period.  
2. Full `isStreakEndEligible`: **every** milestone satisfied for **every** period key in `listPeriodKeysInStreak`.  
3. Dispatch streak-end reward with campaign key above.

Waive of mid-streak milestones still counts toward satisfaction once all periods are done.

---

## 11. Dashboard APIs

### Provider (Driver) — full surface

| Method | Path |
|--------|------|
| GET | `/list` |
| POST | `/create` |
| GET | `/milestone/{journeyId}/list` |
| POST | `/milestone/create` |
| GET | `/stats/history/{driverId}` |
| POST | `/stats/waiveOff` |
| GET | `/driver/{driverId}/assignments` |
| POST | `/cohort/create` |
| GET | `/cohort/list` |
| POST | `/cohortJourney/create` |
| PUT | `/cohortJourney/update` |
| GET | `/cohortJourney/list` |
| POST | `/assign` |
| POST | `/unassign` |
| POST | `/autoApplyCohort/create` |
| PUT | `/autoApplyCohort/update` |
| GET | `/autoApplyCohort/list` |
| POST | `/assign/bulkFromS3` |
| GET | `/assign/bulkFromS3/list` |

Auth: ApiAuthV2. Capability: `system-config.coins.read` / `.write`.

### Rider dashboard — full surface

| Method | Path |
|--------|------|
| GET/POST | `/list`, `/create` |
| GET/POST | `/milestone/{journeyId}/list`, `/milestone/create` |
| GET | `/stats/history/{personId}` |
| POST | `/stats/waiveOff` |
| GET | `/person/{personId}/assignments` |
| POST | `/cohort/create` |
| POST/PUT/GET | `/cohortJourney/create`, `/update`, `/list` |
| POST | `/assign`, `/unassign` |

**Not on rider:** `/cohort/list`, auto-apply*, bulkFromS3*.

### App UI

```
GET .../incentive/journey/list?limit&offset
GET .../incentive/journey/history?date&limit&offset
```

- **List:** only **active-window** enabled CJMs for the person’s assignments; per milestone `currentValue`/`status` for current `periodKey` (missing stats → `NotStarted`). Active before inactive ordering helpers in `Display.hs`.  
- **History:** stats / synthesized rows for the requested date’s period keys.  
- Special-location ids resolved to names when `findSpecialLocationNameById` is wired.

Dashboard + UI share `ServiceHandle` (cache clears, journey/milestone loaders, waive hooks, bulk schedule, timeDiff).

---

## 12. Bulk assign from S3 (provider)

**Multipart** `BulkAssignUserCohortFromS3Req`: `file` (CSV tmp), `s3FilePath` (object key only), `scheduledAt` (required UTC ≥ now), optional `batchSize` (default 500, max 2000), `rescheduleDelaySeconds` (default 2).

**CSV headers:** `userId,cohortMappingId,validTill,enabled`  
(`cohortMappingId` = `cohort_journey_mapping.id`)

**Flow**

1. `putBulkAssignCsv` → S3; insert `bulk_user_cohort_mapping_run` (`Scheduled`); `scheduleBulkUpload` → Allocator `BulkUserCohortMappingUpload` at `scheduledAt` with `attemptCount=1`.  
2. Job: `S3.get` full CSV; parse; process `[offset, offset+batchSize)`; `insertUserCohortMappingIgnoringConflict`; clear assignment cache for parsed users; progress update.  
3. More rows → `createJobIn` next chunk (`attemptCount` reset to 1); else `Succeeded`.  
4. Failures: app-level try/catch, max 5 attempts; CSV parse / S3 NoSuchKey non-retryable → `Failed` + `errorMessage`. Keep job **off** RedisBased `jobRetryOnExceptionMap`.  
5. List API reads run table only (not Redis job keys).

S3 get uses AWS CLI (`aws s3api get-object`); stderr must be captured for diagnosis.

---

## 13. Caching & Redis

TTL generally `cacheConfig.configsExpTime` (idempotency 24h). Cross-app Redis.

**Actor-prefixed** (`driver-offer` | `rider-app`) — cached queries + eval idempotency:

| Purpose | Key pattern |
|---------|-------------|
| Eval idempotency | `{actor}:ride:processed:incentiveJourney:{rideId}` |
| UCM by person | `{actor}:CachedQueries:UserCohortMapping:...` |
| CJM by id | `{actor}:CachedQueries:CohortJourneyMapping:Id-...` |
| Journey by id | `{actor}:CachedQueries:IncentiveJourney:Id-...` |
| Milestones by journey | `{actor}:CachedQueries:IncentiveJourneyMilestone:JourneyId-...` |
| Stats by person+cjm(+milestone)+period | `{actor}:CachedQueries:IncentiveJourneyStats:...` |
| Auto-apply by city+vehicle | `{actor}:CachedQueries:AutoApplyCohortMapping:...` |

**Not actor-prefixed (shared literal):** cash payout lock `IncentiveJourneyCashLock:{referenceId}` in driver SharedLogic. Same key shape for all actors/cities; `referenceId` must stay globally unique for the payout.

Clear assignment (and related) caches on assign / unassign / bulk insert / CJM or auto-apply updates via `ServiceHandle` clears.

---

## 14. End-to-end (driver happy path)

```text
Ops: Journey + Milestones + Cohort + CJM (+ optional AutoApply / Bulk UCM)
Driver completes DynamicOffer valid ride
Kafka RideEnded → processRideEndedCoinsAndJourney
  hasAssignedJourneys? (UCM and/or auto-apply)
    → SETNX rideId → evaluateIncentiveJourneys
         load assignments → filter active streak window
         for each journey: periodKey + sequential milestones
           filters → deltas → upsert stats → dispatch reward → overlay
         maybe streak-end award
    else legacy Incentive# coins
Driver app: GET list / history
Ops: stats history / waiveOff / bulk run list
```

---

## 15. Invariants & gotchas

1. Journey assignment (UCM **or** auto-apply) **replaces** legacy EndRide coins on DynamicOffer for the whole assignment — including when outside the streak window (eval no-ops; legacy stays suppressed).  
2. Filter failure (time/vehicle/location) **stops** the milestone chain for that ride.  
3. Completing a milestone does not double-count the ride on the next; `RewardPending` retry does allow continuation.  
4. Progress is per **(person, CJM, milestone, periodKey)** — same journey template can run under multiple campaigns.  
5. OTP path: **no journey eval**; assignment only gates legacy coins.  
6. `BookingTicket` delta is 0 on the ride evaluation path until wired.  
7. Weekly/Monthly `startDate` validation at CJM create/update.  
8. Eval writes `Completed` (not `Rewarded`); `Rewarded` remains a terminal/satisfied enum for reads/legacy.  
9. Location filters with `Pickup`/`Drop`/`PickupDrop` require a non-empty `specialLocationIds` list or the filter fails.  
10. Rider payouts are largely deferred; do not assume Coins/Cash work on rider-app.  
11. Cash without VPA stays `RewardPending` until `retryVerifiedVpaJourneyCashPayouts`.  
12. Bulk job must not use scheduler `jobRetryOnExceptionMap` on RedisBased allocators.

---

## 16. File cheat sheet

```
lib/incentive-journey/
  Types.hs, Types/Actor.hs, Display.hs, Reward.hs, Convert.hs, Common.hs
  Eval.hs, Condition.hs, Window.hs, Period.hs
  Streak.hs, StreakEnd.hs, Idempotency.hs, Assignment.hs
  Domain/Action/Evaluate.hs
  Domain/Action/Dashboard/{Core,Provider,Rider,ServiceHandle}.hs
  Domain/Action/UI/IncentiveJourney.hs
  Domain/Types/{Common,BulkAssignUserCohortFromS3}.hs
  Storage/CachedQueries/*.hs
  Storage/Queries/*Extra.hs
  spec/Storage/*.yaml                    # exactly 8 tables
  spec/IncentiveJourney/{ProviderPlatform,RiderPlatform}/API/IncentiveJourney.yaml

dynamic-offer-driver-app/.../
  SharedLogic/IncentiveJourney.hs        # rewards, overlays, VPA retry, assignment wrapper
  SharedLogic/RideEvents/DriverCoinsAndJourney.hs
  SharedLogic/Allocator/Jobs/IncentiveJourney/BulkUserCohortMappingUpload.hs
  Domain/Action/{UI,Dashboard}/IncentiveJourney/...
  Domain/Action/UI/Payout.hs             # calls retryVerifiedVpaJourneyCashPayouts
  Allocator App.hs

rider-app/.../
  SharedLogic/IncentiveJourney.hs
  Domain/Action/Beckn/Common.hs
  Domain/Action/Dashboard/IncentiveJourney/...

kafka-consumers/.../Processor/RideEvents/Handlers.hs
```
