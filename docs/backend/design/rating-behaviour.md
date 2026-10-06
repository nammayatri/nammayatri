# RATING_BEHAVIOR — low-rating inform → warn → soft block → block

Status: implemented (code), rules NOT yet rolled out to any city.
Date: 2026-10-06

## Why

Driver star-ratings have no automated consequence today. A production audit (2026-10-06)
found the behaviour engine live for exactly one domain (GPS-TOLL-BEHAVIOR, Bangalore only);
all other automated blocking comes from the legacy cancellation-rate slab config in
`transporter_config`. Bangalore has ~1,600 active-ish cab drivers below 4.0 (≥5 rated rides)
with no systematic treatment.

## What was added (code)

| Change | File |
|---|---|
| New `RATING_BEHAVIOR` logic domain | `lib/yudhishthira/src/Lib/Yudhishthira/Types.hs` (constructor, `Enumerable`, Show/Read as `RATING-BEHAVIOR`, show-instances list) |
| New behaviour module: counter config + pipeline trigger | `Main/src/SharedLogic/BehaviourManagement/LowRating.hs` |
| Trigger hook on every rating ingestion (forked, non-blocking) | `Main/src/Domain/Action/Beckn/Rating.hs` (`handler`, after `calculateAverageRating`) |
| Visibility registry (counters + reason tags on `behaviorVisibility` dashboard API) | `Main/src/SharedLogic/BehaviourManagement/Visibility.hs` |
| Dashboard rule authoring/verification + domain schema | `Main/src/Domain/Action/Dashboard/Management/NammaTag.hs` (verify + schema branches) |
| `LowRating` block reason flag + tag mapping (`LOW_RATING_BLOCK`) | `Main/src/Tools/Error.hs`, `ConsequenceDispatcher.hs` (`parseBlockReasonFlag`) |
| Bulk block API `POST /driver/bulkBlock` | `Main/spec/ProviderPlatform/Management/API/Driver.yaml` + `Domain/Action/Dashboard/Management/Driver.hs` (`postDriverBulkBlock`, shared `blockDriverWithReason` core, cap 500/call) |

Run `, run-generator` (for the Driver.yaml API spec) and `cabal build all` after pulling.

## Event & counter model

Action type: `LOW_RATING`. Counters (Redis sliding window, 90-day storage):

- `ELIGIBLE_COUNT` — every **new** rating received (rules increment it via `INCREMENT_COUNTER`)
- `ACTION_COUNT` — every new rating `<= 3` stars
- Periods in snapshot: `weekly` (7d), `monthly` (30d), `quarterly` (90d);
  `rate = actionCount*100 / max 1 eligibleCount`

Snapshot fields available to rules:

```
eventData.ratingValue        -- 1..5 for this ride
eventData.isNewRating        -- false when the rider edited an existing rating
eventData.rideId
entityState.lifetimeRating      -- driver_stats score/count, e.g. 4.63 (null until first rating)
entityState.lifetimeRatingCount -- lifetime rated-ride count
counters.{weekly,monthly,quarterly}.{actionCount,eligibleCount,rate}
cooldowns.<tag>              -- true while a block cooldown is active
```

Ratings are sparse (even active low-rated drivers get only a few per week), so rules
should gate on `monthly`/`quarterly` counters and lifetime figures — never `daily`.

Throttling notes:
- `SOFT_BLOCK` / `HARD_BLOCK` write cooldown keys (`cooldownHours` param) → guard re-blocking
  with `{"var": "cooldowns.LOW_RATING_SOFT_BLOCK"}` / `cooldowns.LOW_RATING_BLOCK`.
- `NUDGE` / `WARN` do **not** write cooldowns → throttle them by firing only at an exact
  `monthly.actionCount` value (fires once per threshold crossing per window).

## Suggested ladder (thresholds from the 2026-10 Bangalore data)

| Rung | Condition (all on a fresh low-rating event) | Consequence |
|---|---|---|
| Inform | lifetime < 4.3, ≥10 rated rides, 2nd low rating this month | `NUDGE` overlay `LOW_RATING_NUDGE` |
| Warn | lifetime < 4.0, ≥15 rated rides, 3rd low rating this month | `WARN` overlay `LOW_RATING_WARN` |
| Soft block | lifetime < 3.8, ≥15 rated rides, ≥5 low ratings and ≥40% low-rate this month | `SOFT_BLOCK` premium tiers 72h, cooldown 168h |
| Hard block | lifetime < 3.0, ≥20 rated rides | `HARD_BLOCK` 72h, cooldown 336h |

Rationale: with ≥5 rated rides, only 0.08% of Bangalore cab drivers sit below 3.0 and ~1.5%
in 3–4, so these thresholds touch the extreme tail only; the ≥15–20 rated-rides gates keep
single-ride noise out (a driver with 5 ratings swings 0.8 stars on one 1-star).

## Sample rulebook (3 elements, Bangalore)

Written in the production JsonLogic idiom (multi-key objects built with `cat` of
single-key objects; `runLogics` folds elements, each element's output is the next's input;
the final element must output `{"consequences": [...], "communications": [...]}`).

Element 0 — derive helper fields:

```json
{"cat": [
  {"var": ""},
  {"isNewRating": {"==": [{"var": "eventData.isNewRating"}, true]}},
  {"isLowRating": {"and": [
    {"==": [{"var": "eventData.isNewRating"}, true]},
    {"<=": [{"var": ["eventData.ratingValue", 5]}, 3]}]}},
  {"lifetimeRating": {"var": ["entityState.lifetimeRating", 5]}},
  {"ratedRides": {"var": ["entityState.lifetimeRatingCount", 0]}}
]}
```

Element 1 — pick the escalation rung (counters are pre-event values; this event's
increment happens via INCREMENT_COUNTER consequences, so `actionCount == 1` means
"this is the 2nd low rating this month"):

```json
{"cat": [
  {"var": ""},
  {"rung": {"if": [
    {"!=": [{"var": "isLowRating"}, true]}, "NONE",
    {"and": [
      {"<": [{"var": "lifetimeRating"}, 3.0]},
      {">=": [{"var": "ratedRides"}, 20]},
      {"!=": [{"var": ["cooldowns.LOW_RATING_BLOCK", false]}, true]}
    ]}, "HARD_BLOCK",
    {"and": [
      {"<": [{"var": "lifetimeRating"}, 3.8]},
      {">=": [{"var": "ratedRides"}, 15]},
      {">=": [{"var": ["counters.monthly.actionCount", 0]}, 4]},
      {">=": [{"var": ["counters.monthly.rate", 0]}, 40]},
      {"!=": [{"var": ["cooldowns.LOW_RATING_SOFT_BLOCK", false]}, true]},
      {"!=": [{"var": ["cooldowns.LOW_RATING_BLOCK", false]}, true]}
    ]}, "SOFT_BLOCK",
    {"and": [
      {"<": [{"var": "lifetimeRating"}, 4.0]},
      {">=": [{"var": "ratedRides"}, 15]},
      {"==": [{"var": ["counters.monthly.actionCount", 0]}, 2]}
    ]}, "WARN",
    {"and": [
      {"<": [{"var": "lifetimeRating"}, 4.3]},
      {">=": [{"var": "ratedRides"}, 10]},
      {"==": [{"var": ["counters.monthly.actionCount", 0]}, 1]}
    ]}, "NUDGE",
    "NONE"
  ]}}
]}
```

Element 2 — emit counter increments + rung consequence:

```json
{"cat": [
  {"consequences": {"if": [
    {"!=": [{"var": "isNewRating"}, true]}, [],
    {"==": [{"var": "rung"}, "HARD_BLOCK"]}, [
      {"cat": [{"consequenceType": "INCREMENT_COUNTER"}, {"params": {"cat": [{"counterType": "ELIGIBLE_COUNT"}]}}, {"requiresResolution": false}]},
      {"cat": [{"consequenceType": "INCREMENT_COUNTER"}, {"params": {"cat": [{"counterType": "ACTION_COUNT"}]}}, {"requiresResolution": false}]},
      {"cat": [{"consequenceType": "HARD_BLOCK"}, {"params": {"cat": [{"blockDurationHours": 72}, {"blockReason": "Repeated low customer ratings"}, {"blockReasonTag": "LOW_RATING_BLOCK"}, {"cooldownHours": 336}]}}, {"requiresResolution": false}]}
    ],
    {"==": [{"var": "rung"}, "SOFT_BLOCK"]}, [
      {"cat": [{"consequenceType": "INCREMENT_COUNTER"}, {"params": {"cat": [{"counterType": "ELIGIBLE_COUNT"}]}}, {"requiresResolution": false}]},
      {"cat": [{"consequenceType": "INCREMENT_COUNTER"}, {"params": {"cat": [{"counterType": "ACTION_COUNT"}]}}, {"requiresResolution": false}]},
      {"cat": [{"consequenceType": "SOFT_BLOCK"}, {"params": {"cat": [{"blockDurationHours": 72}, {"blockedFeatures": []}, {"blockedServiceTiers": ["SUV", "SEDAN", "SUV_PLUS", "TAXI_PLUS", "EV_SEDAN", "HERITAGE_CAB", "VIP_OFFICER"]}, {"blockReason": "Low customer rating - premium tiers restricted"}, {"blockReasonTag": "LOW_RATING_SOFT_BLOCK"}, {"cooldownHours": 168}]}}, {"requiresResolution": false}]}
    ],
    {"==": [{"var": "rung"}, "WARN"]}, [
      {"cat": [{"consequenceType": "INCREMENT_COUNTER"}, {"params": {"cat": [{"counterType": "ELIGIBLE_COUNT"}]}}, {"requiresResolution": false}]},
      {"cat": [{"consequenceType": "INCREMENT_COUNTER"}, {"params": {"cat": [{"counterType": "ACTION_COUNT"}]}}, {"requiresResolution": false}]},
      {"cat": [{"consequenceType": "WARN"}, {"params": {"cat": [{"warnKey": "LOW_RATING_WARN"}, {"showOnProfile": false}]}}, {"requiresResolution": false}]}
    ],
    {"==": [{"var": "rung"}, "NUDGE"]}, [
      {"cat": [{"consequenceType": "INCREMENT_COUNTER"}, {"params": {"cat": [{"counterType": "ELIGIBLE_COUNT"}]}}, {"requiresResolution": false}]},
      {"cat": [{"consequenceType": "INCREMENT_COUNTER"}, {"params": {"cat": [{"counterType": "ACTION_COUNT"}]}}, {"requiresResolution": false}]},
      {"cat": [{"consequenceType": "NUDGE"}, {"params": {"cat": [{"nudgeKey": "LOW_RATING_NUDGE"}]}}, {"requiresResolution": false}]}
    ],
    {"==": [{"var": "isLowRating"}, true]}, [
      {"cat": [{"consequenceType": "INCREMENT_COUNTER"}, {"params": {"cat": [{"counterType": "ELIGIBLE_COUNT"}]}}, {"requiresResolution": false}]},
      {"cat": [{"consequenceType": "INCREMENT_COUNTER"}, {"params": {"cat": [{"counterType": "ACTION_COUNT"}]}}, {"requiresResolution": false}]}
    ],
    [
      {"cat": [{"consequenceType": "INCREMENT_COUNTER"}, {"params": {"cat": [{"counterType": "ELIGIBLE_COUNT"}]}}, {"requiresResolution": false}]}
    ]
  ]}},
  {"communications": []}
]}
```

## Rollout checklist (per city)

1. Seed `merchant_overlay` rows for PN keys `LOW_RATING_NUDGE` and `LOW_RATING_WARN`
   (per language) — `NUDGE`/`WARN` silently no-op without them.
2. Create the rulebook via dashboard: `POST .../nammaTag/appDynamicLogic/verify` with
   `domain = RATING-BEHAVIOR` (elements above), then roll out via the rollout API
   (start at a small percentage; rules are per-city opt-in — cities without rules no-op).
3. Observe via `GET .../nammaTag/behaviorVisibility/DRIVER/{driverId}` (`LOW_RATING`
   counters, `LOW_RATING_*` blocks/cooldowns) and `driver_block_transactions`
   (`block_reason_flag = LowRating`).
4. Recommended phasing: nudge+warn rungs only for 2–4 weeks → add soft block → add hard
   block once nudge volume and false-positive rate look sane.

## Bulk block API (one-time cleanup)

`POST /driver/bulkBlock` (capability `city-operations.driver_block.write`), body:

```json
{
  "driverIds": ["<id>", "..."],
  "reasonCode": "others",
  "blockReason": "Blocked for sustained low customer rating",
  "blockTimeInHours": 168
}
```

Max 500 ids per call; response `{success, failed, failedItems: [{driverId, errorMessage}]}` —
already-blocked / not-found / wrong-city drivers land in `failedItems` without aborting the
batch. Each success writes a `driver_block_transactions` audit row and schedules auto-unblock
when `blockTimeInHours` is set. Consider seeding a dedicated `driver_block_reason` row
(e.g. `LOW_RATING`) instead of `others` so the audit trail is queryable.
