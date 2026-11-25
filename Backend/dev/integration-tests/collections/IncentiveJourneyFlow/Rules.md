# IncentiveJourneyFlow — Integration Test Rules

Provider/driver **Incentive Journey** E2E for **NAMMA_YATRI** (Bangalore): dashboard creates a Daily journey with two ride milestones (each `RideCompleted` GTE **1**) and one `Earnings` milestone the rides do not finish. The same cohort gets a second journey (`RideCompleted` GTE **2**, 200 coins) through a second cohort-journey mapping and a second assign. A different cohort and journey are auto-applied for `AUTO_CATEGORY` (the onboarded driver's vehicle) with no user mapping. Two AUTO cash rides complete those ride targets, the earnings milestone is waived, and the first mapping's streak-end reward pays cash ₹1. Milestone 1 is a YATRI_SUBSCRIPTION waive-off instead of coins.

## Prerequisites

0. **Code cutover** — see [`Backend/lib/incentive-journey/CUTOVER.md`](../../../../lib/incentive-journey/CUTOVER.md): `, run-generator`, delete temp API shims, apply new `Local_API_IncentiveJourney*.sql`, `cabal build`.

1. **Services** (from `Backend/`, nix shell):
   ```bash
   , run-mobility-stack-full
   ```
   Must include `dynamic-offer-driver-app` (`:8016`), `driver-offer-allocator`, `provider-dashboard` (`:8018`), `rider-app` (`:8013`), Postgres (`:5434`), Redis, Kafka, **`kafka-ride-events-consumer-exe`**, **`kafka-ij-behavior-tick-consumer-exe`**, and **mock-server** (`:8080`). Suite `02` sends the CSV as a multipart file on `POST .../assign/bulkFromS3`. Driver-app writes that object under `Backend/s3/local/test-bucket/<key>`, and the allocator reads it from there. The mock `put-text` step also writes `collections/IncentiveJourneyFlow/fixtures/bulk-assign.csv` so newman can attach that file. Restart mock-server after pulling that endpoint.

2. **Consumers** —
   - Ride-end: `kafka-ride-events-consumer-exe` (`RIDE_EVENTS_CONSUMER` / `DriverCoinsAndJourney`). Without it, ride-end milestone polls fail.
   - Behaviour ticks: `kafka-ij-behavior-tick-consumer-exe` (`IJ_BEHAVIOR_TICK_CONSUMER`, Redis stream `dynamic-offer-driver-app:ij_behavior_tick_stream_*`). Accept publishes via `publishBehaviorMetricTick`; without it, suite `03`'s BehaviourMetricTick-only award poll fails. Restart the stack (or start that process) after pulling this wiring.

3. **Dashboard auth** (auto-run by `./run-tests.sh incentive-journey`, or after DB reset):
   ```bash
   psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev \
     -f Backend/dev/local-testing-data/provider-dashboard.sql \
     -f Backend/dev/migrations-read-only/dashboard/API_IncentiveJourney_IncentiveJourney.sql \
     -f Backend/dev/migrations-read-only/dashboard/Local_API_IncentiveJourney_IncentiveJourney.sql
   ```
   The last two files map `PROVIDER_INCENTIVE_JOURNEY/*` endpoints to `system-config.coins.*` and grant those capabilities to the local admin role. Without them, every dashboard IJ call returns 403.

3b. **Driver wallet (Cash/NetOff)** — `local-testing-data/dynamic-offer-driver-app.sql` §14 sets Bangalore `enableDriverWallet` + Liability/`DRIVER` `transfer_in_enabled` after config-sync (S3 `master_to_local` often ships wallet-off / omits `finance_account_config`). Re-seed that file after sync; clear Redis `Finance:AccountConfig:7f7896dd-…` or restart driver-app. Collections still set VPA as payout fallback.

## Peer URL path (after generator cutover)

Dashboard APIs are **not** under `management`. After the IssueManagement-style peer move:

```
{{dashboard_base_url}}/bpp/driver-offer/{{dashboard_merchant_id}}/{{city}}/incentiveJourney/...
```

Examples:

| Action | Method + path |
|--------|----------------|
| Create cohort | `POST .../cohort/create` |
| Create journey | `POST .../create` (name, description, journeyType only) |
| Create milestone | `POST .../milestone/create` |
| List milestones | `GET .../milestone/{journeyId}/list` |
| Cohort↔journey map | `POST .../cohortJourney/create` (enabled, maxWaiveOffCount, streak-end reward) |
| Roll next campaign window | `POST .../cohortJourney/roll` (`cohortJourneyMappingId`, new `startDate`; copies streak/rewards, sets `parentId`) — covered in `01` |
| List cohort↔journey maps | `GET .../cohortJourney/list` (includes `parentId`) |
| Assign driver | `POST .../assign` (one user mapping per cohort-journey mapping) |
| Unassign | `DELETE .../unassign` |
| Disable cohort mapping | `PUT .../cohortJourney/update` (`cohortJourneyMappingId`, `enabled: false`) |
| Auto-apply a mapping for the driver's category | `POST .../autoApplyCohort/create` (`vehicleCategory` `AUTO_CATEGORY`, `allowIfNoMapping` false) |
| Disable auto-apply | `PUT .../autoApplyCohort/update` (`enabled: false`) |
| Driver assignments | `GET .../driver/{driverId}/assignments` |
| Stats history | `GET .../stats/history/{driverId}?fromDate=&toDate=&journeyId=` |
| Waive milestone | `POST .../stats/waiveOff` (`driverId`, `journeyId`, `milestoneId`, `periodKey`) |
| Inspect / catch-up streak-end | `POST .../streakEnd/inspect` (`driverId`, `cohortJourneyMappingId`, `periodKey`, `trigger`) — periodized: `SliceNotClosed` until exclusive slice end; classic NetOff: same vs `computeStreakEndDate`; classic Cash/Coins: ready on last period day; NetOff streak-end → Cash |
| Set driver payout VPA (cash fallback) | `POST .../payout/payout/vpa/update` (`personId`, `vpa`, `verify: false`, `vpaStatus: VERIFIED_BY_USER`) — needed when wallet credit is **not** used (wallet off or `transfer_in_enabled=false`). When `enableDriverWallet` + `transfer_in_enabled`, Cash/NetOff credits wallet (`IncentiveJourneyCash`) without VPA; else bank/UPI payout. `MANUALLY_ADDED` leaves `RewardPending` on payout path |

There is no journey update API, and no delete for a cohort-journey mapping. Set `enabled: false` on `PUT .../cohortJourney/update`. Eval and the driver list already skip a disabled mapping. Stats rows stay.

Driver coins balance (token = driver session):

- `GET {{baseURL_namma_P}}/coins/transactions?date=` — `coinBalance` before and after waive (streak-end is cash ₹1, so the balance does not increase)

Driver UI (token = driver session):

- `GET {{baseURL_namma_P}}/incentive/journey/list`
- `GET {{baseURL_namma_P}}/incentive/journey/history`

Auth header for dashboard: `token: {{dashboard_token}}` (refreshed by `switchMerchantAndCity`).

## Running

```bash
cd Backend/dev/integration-tests
./run-tests.sh incentive-journey              # Bangalore (only Local env)
./run-tests.sh incentive                      # alias
./run-tests.sh incentive-journey NY_Bangalore
./run-tests.sh incentive-journey NY_Bangalore 01-IncentiveJourneyRideProgressFlow
./run-tests.sh incentive-journey NY_Bangalore 02-IncentiveJourneyBulkAssignRideProgressFlow
./run-tests.sh incentive-journey NY_Bangalore 03-IncentiveJourneyMgCampaignOptInFlow
```

Skip automatic SQL seed: `NY_TEST_SKIP_INCENTIVE_SEED=1 ./run-tests.sh incentive-journey`

Newman directly:

```bash
newman run collections/IncentiveJourneyFlow/01-IncentiveJourneyRideProgressFlow.json \
  -e collections/IncentiveJourneyFlow/Local/Local_NY_Bangalore.postman_environment.json \
  --bail --timeout-request 60000
```

## Collection

| `01-IncentiveJourneyRideProgressFlow.json` | One cohort, two journeys (two mappings, two `/assign` calls). Journey 1 milestones stay legacy simple columns. Journey 2 uses **evaluation.logic** JsonLogic `ridesCompleted>=2 AND (earnings>=1 OR distanceMeters>=1)` (200 coins) on ride 2. Auto-apply uses the same AND/OR shape with `ridesCompleted>=1` (30 coins) on ride 1. Polls assert `currentMetrics`. Then waive earnings → streak-end cash ₹1. Before cleanup: roll campaign checks. Then unassign / disable mappings |
| `02-IncentiveJourneyBulkAssignRideProgressFlow.json` | Same ride progress as `01`, except both user mappings are inserted by one `POST .../assign/bulkFromS3` job (`batchSize` 1, so two allocator ticks). That request uploads the CSV as a multipart file, and driver-app stores it at the given S3 key. Roll API is covered in `01` only |
| `03-IncentiveJourneyMgCampaignOptInFlow.json` | **MG OptIn + NetOff**. One journey / one `NetOffEarnings` milestone with full `rewardMetadata` slabs. CJM `OptIn` + `controlFeatures`, `streakRange=2`, `startDate=yesterday`. Invite → opt-in → ONLINE → **Local open-session onlineMinutes flush** (backdate OnlineHours + list → poll `ij:bm:onlineMinutes` today) → Accept/ride **stats-sync only** (`InProgress`) → Redis-seed closed `Day:` ij:bm → list settle **NetOff Cash** → unassign → re-invite → dashboard opt-in → cleanup |

## OptIn + controlFeatures suite (`03`)

Do **not** fold this into `01` — Assigned ride progress and OptIn+controls are different flows.

`03` is self-contained and covers:

1. **One milestone** — `rewardType=NetOffEarnings` + real bag `evaluation.logic`: `ridesCompleted>=0 AND (acceptanceRate>=0 OR onlineMinutes>=0)` (UI/shape only — NetOff settle does **not** gate on condition; slabs drive payout). Full **`rewardMetadata`**: `includeEarningsAllDay`, `mgAmountSlabs` (onlineMinutes→MG), `percentSlabs` (acceptanceRate + cancellationRate), `violationPercentSlabs` + `violationMetrics` (`dxCancel`/`dafCancel`). Fallback `rewardValue` when slabs empty.
2. CJM **`campaignUserLinkType=OptIn`** + **`controlFeatures`**. **`streakEndRewardType=NetOffEarnings`** with the same slabs in **`streakEndRewardMetadata`** (explicit **`considerExtraEarning: true`**), **`periodDays=1`** (`streakRange=2`). Closed-day list settle pays **milestone** NetOff Cash **and** the **1-day periodized streak** NetOff Cash (ops may configure both; prod often uses milestone `NoReward` + streak-only NetOff). Behaviour milestones / NetOff keep `trackBehaviorMetrics` on so Accept/cancel still bump ij:bm.
3. Opt-in **before** invite must fail; invite assign; driver list shows **Pending** OptIn (`campaignUserLinkType` / `optInStatus` / `validTill`) before opt-in
4. Driver UI `/incentive/journey/optIn` then `GET /driver/profile` asserts `driverTags["IJCampaign:<cjmId>"]` starts with `OptIn;daily` (no milestone timeBounds → `daily`). Then list/history/assignments assert exact `controlFeatures` payloads (`CanDxCancelBooking=false`, `AutoAssign` locked ON, `PoolingPriority HIGH`). List/history milestones also expose **`mgSlabs`** (MG ladder only — not full `rewardMetadata`). `GET /driver/vehicleServiceTiers` → `autoAcceptLockedByFleet=true`. Nearby SRD → `driverCancellationNotAllowed=true`; post-accept cancel → non-2xx. (Pooling sort / silent-assign fire are not E2E-asserted.)
4b. **Local open-session onlineMinutes flush** (after `setActivity` ONLINE): backdate `driver-offer:OnlineHours:{driver}` `lastOnlineAt` by ~2m + clear `ij:online:flush:{driver}` → `GET /incentive/journey/list` → poll `ij:bm:onlineMinutes:{driver}:{cjm}:{Day:today}:daily` until `>= 1`. Asserts Redis bank from the list settle fork without waiting a real 60s. Do **not** assert list `currentMetrics.onlineMinutes` on that response (fork + tick→stats lag). Closed-day settle seed (`onlineMinutes=180` on `Day:yest`) is unchanged and still separate.
5. Mid-period: behaviour + ride **stats-sync only** (`InProgress`, `slabPercent` present). NetOff settles the **previous closed period** via **list-API fork only**. Watermark `ij:settle:done:{person}:{cjm}`; catchup lock must release (or seed deletes it). NetOff payout ≤ 0 → Completed 0; payout > 0 + AwardSkipped → `RewardPending` (wallet off/`transfer_in` off and no verified VPA). Cancel/accept rates from period ij:bm/stats.
5b. **After closed-day settle** (does not touch seeded `Day:yest`): second search → accept → **rider cancel** (customer-at-fault in this flow). Assert current-period **`ij:bm:dafCancel==0`** and **`dxCancel==0`** (daf = driver-at-fault only; rider cancel does **not** bump when customer is at fault), then stats Prefer `*InsideTimebound`. After 1 completed ride expect **rrReceived/rrAccepted ≈ 1** (primary-only INCR; no once-keys — each SRD batch send counts for `rrReceived`; accept/cancel gated by domain). Closed-day seed `Day:yest` is a different periodKey. `acceptanceRate` ≤ 100.
6. NetOff **daily** settle: `max(0, (mg − earnings) * minPercent / 100)`; `minPercent` = **minimum** across that row’s `percentSlabs` / `violationPercentSlabs`. Streak NetOff (classic or `periodDays`): per period, if `(mg−earnings)>0` apply that row’s %; else if `considerExtraEarning` (default **true**) keep raw negative, if **false** contribute 0; Cash = `max(0, sum)`. Do **not** add `ridesCompleted` percent slabs in local `03` (closed-day seed has 0 rides).
7. Unassign → list clears; re-invite → assignments assert **Pending** / `optedInAt=null` (unassign cleared opt-in) → dashboard `/optIn`; cleanup disable

`01` remains the broad ride/JsonLogic/auto-apply/waive/roll suite. `03` is the MG campaign shape (OptIn + controls + single NetOff).

### Local settle hook in `03` (no Haskell / clock change)

Live Accept/ride always write **today’s** `periodKey`. Settle needs a **previous** key in-streak. Collection does:

1. `streakRange=2` + `startDate` = yesterday IST → period keys `[Day:yest, Day:today]`.
2. Mid-period polls assert **`InProgress` + `currentMetrics`** (not `Completed`).
3. Seed `ij:bm:rrReceived|rrAccepted|onlineMinutes` for `Day:yest` and delete `ij:settle:done:…` + `ij:settle:…:catchup`.
4. `GET /incentive/journey/list` → poll closed `Day:yest` NetOff row **`Completed`** with Cash `rewardValue` from slabs (`onlineMinutes=180` → MG 1000 × 100% when seeded accept is good and earnings empty).

Requires `envType=Local` + mock-server. Cash/NetOff: after config-sync, Bangalore wallet + `transfer_in_enabled` → wallet credit (`IncentiveJourneyCash`); if `finance_account_config` was never synced, verified VPA is needed for payout fallback. Without the Redis seed step, closed-day rates/MG stay empty and payout may be 0.

**Membership / controls:** wire `IJCampaign:<cjmId>#Assigned|OptIn;daily|tb[#expiry]` (cjmId only in tag name; `daily`/`tb` are Redis suffixes only — real windows live on milestone/CJM cache). Tag `#expiry` is **real UTC** (local UCM.validTill converted on write). At most one **live** **controlFeatures** campaign per driver (ended CJMs do not occupy the slot even if UCM.validTill is still open). Dashboard `GET .../assignments` and driver `GET .../incentive/journey/list` expose `optInStatus` (`NotRequired` | `Pending` | `OptedIn`) plus `campaignUserLinkType` / `optedInAt` (list also returns `validTill` for POST `/optIn`). Pending invites appear on the driver list for the opt-in popup only — eval/settle still require `optedInAt`. **Unassign** clears `optedInAt` → re-invite is **Pending** (fresh opt-in). **OptIn** tag on opt-in only; **all Assigned** tags ensured in a non-blocking list fork (never on assign/bulk). Pooling/cancel/AA resolve **CJM.controlFeatures** via pod in-mem cache; each feature’s **`enableIn`** (`AllDay` | `TimeBound`, default `TimeBound`) gates that perk independently against first-milestone `timeBounds`. Unexpired membership always suppresses fleet AA/cancel/pooling — **behaviour-only** (no controlFeatures) → driver defaults only; unassign to restore fleet. **ij:bm / BehaviourMetricTick** only when the journey has behaviour-related milestones (`trackBehaviorMetrics`); controlFeatures-only or ride/earnings-only campaigns keep the tag but skip Redis bumps, Kafka ticks, and behaviour stats sync. Redis key is period-scoped: `ij:bm:{metric}:{driver}:{cjm}:{Day|Week|Month:…}:{daily|tb}` (same periodKey as stats rows). Key ownership: `Lib.BehaviorTracker.CampaignCounters` (city rates stay on `bt:` via `Accumulator`). BT is a **library** — `ij:bm` INCR still runs in-process on driver-app (Domain → SharedLogic → BT); the behaviour-tick consumer only syncs Postgres stats, it does **not** own bumps. Moving bump code into the BT package is not a load win. **Stats is progressive SoT** for settle/reward (max-merge with Redis; never clobber DB with zeros). On Redis key miss, bump re-seeds from stats before INCR.

Run alone: `./run-tests.sh incentive-journey NY_Bangalore 03-IncentiveJourneyMgCampaignOptInFlow`

## Bulk assign suite

`02` keeps the same journeys, rides, waive, and cleanup. It does not call `POST .../assign`. After both cohort-journey mappings exist it:

1. Builds a two-row CSV (`userId,cohortMappingId,validTill,enabled`) from the driver id and both cohort-journey mapping ids, and writes a fixture with `POST {{mockServerUrl}}/mock/s3/put-text`.
2. `POST .../assign/bulkFromS3` as `multipart/form-data`: file part `file` (built from those ids, no manual upload), plus `s3FilePath`, `scheduledAt` about 15 seconds ahead, `batchSize` 1, `rescheduleDelaySeconds` 2. Driver-app uploads that file to the object key, then schedules the job.
3. Polls `GET .../assign/bulkFromS3/list` until that `runId` is `Succeeded` with `rowsInserted` 2.

`scheduledAt` must be in the future or the scheduler rejects the job. The allocator has to be up to consume it. A row that already exists for the same driver and mapping is skipped, not re-enabled.

## Milestone shape in this collection

Eval does not carry progress into the next milestone, and it does not start the next milestone in the same ride. After milestone 1 is completed, the next ride starts milestone 2 at 0.

| Order | Condition | Value | What the test does |
|-------|-----------|-------|--------------------|
| 1 | RideCompleted GTE | 1 | Completed by ride 1 (100% YATRI_SUBSCRIPTION waive-off, 1 day, WITHOUT_OFFER) |
| 2 | RideCompleted GTE | 1 | Still open after ride 1; completed by ride 2 (10 coins) |
| 3 | Earnings GTE | 1000000 | Left open, then `POST .../stats/waiveOff` |

Same cohort, second mapping (separate assign; a user mapping points at one cohort-journey mapping, not the whole cohort) — **JsonLogic bag eval on ride-end**:

| Order | Condition | Value | What the test does |
|-------|-----------|-------|--------------------|
| 1 | `conditionLogic`: `ridesCompleted >= 2` **AND** (`earnings >= 1` **OR** `distanceMeters >= 1`) | 2 | Open at `currentValue=1` after ride 1; completed on ride 2 (200 coins). Snapshot includes rides + earnings/distance vars |

Separate auto-apply cohort (no user mapping, `allowIfNoMapping` false so it still applies while the driver has the other mappings, `vehicleCategory` `AUTO_CATEGORY`, matching Add Vehicle) — **JsonLogic**:

| Order | Condition | Value | What the test does |
|-------|-----------|-------|--------------------|
| 1 | `ridesCompleted >= 1` **AND** (`earnings >= 1` **OR** `distanceMeters >= 1`) | 1 | Completed by ride 1 (30 coins) + compound `metricSnapshot` |

`periodKey` is taken from the completed ride stats row (`Day:YYYY-MM-DD`, city local day). Cohort mapping `startDate` is noon UTC on that same IST calendar day so streak-end (streakRange 1, cash ₹1) matches the ride period. `maxWaiveOffCount` is 1.

## Idempotency

- Random `_test_driver_number`, `_test_rider_number`, `_test_reg_no`, `_test_ij_suffix` per run (collection prerequest).
- Journey/cohort names: `INT_TEST_IJ_*_{{suffix}}`.
- Never hardcode phones, reg nos, or entity ids.

## Env notes

- `ij_eval_wait_ms` (default `12000`) — busy-wait before ride-end list/stats polls so Kafka can catch up.
- `ij_behaviour_tick_wait_ms` (falls back to `ij_eval_wait_ms`) — wait after Accept before BehaviourMetricTick stats poll.
- `_test_start_date` — noon UTC of today's IST day, so streak period keys match ride eval.
- `envType=Local` enables mock-server steps; non-Local auto-skips URLs containing `mockServerUrl` / `mock_fcm_url`.
