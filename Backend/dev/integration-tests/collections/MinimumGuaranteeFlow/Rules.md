# MinimumGuaranteeFlow — Integration Test Rules

Minimum Guarantee (MG) campaign lifecycle for **NAMMA_YATRI** (Bangalore): the dashboard creates, edits, invites to and publishes a campaign; a freshly onboarded AUTO driver opts in and out for **tomorrow**; the campaign is paused, extended and cancelled; a second campaign proves the budget cap. No rides are completed (see "Not covered").

## Prerequisites

1. **Services** (from `Backend/`, nix shell): `dynamic-offer-driver-app` (`:8016`), `provider-dashboard` (`:8018`), Postgres (`:5434`), Redis. Redis must be reachable by the driver app: the per-driver MG context, the budget counter and the pause flag live there.
2. **Tables and dashboard access** — `./run-tests.sh minimum-guarantee NY_Bangalore` (or `./run-tests.sh --setup`) applies, when missing:
   - `Backend/dev/migrations-read-only/dynamic-offer-driver-app/mg_{campaign,campaign_window,campaign_rule,eligibility,enrollment}.sql`
   - `Backend/dev/migrations-read-only/dashboard/{API,Local_API}_{Management,Fleet}_MinimumGuarantee.sql` (capability endpoints plus local role grants; they reuse `system-config.coins.read/write` and `fleet.driver.read/write`)
   - `local-testing-data/dashboard.sql` (admin token, merchant access)
3. The driver app must be built from the `backend/feat/mg-campaign` branch so the `/driver/mg/*` and `…/minimumGuarantee/*` routes exist.

## Run

```bash
cd Backend/dev/integration-tests
./run-tests.sh minimum-guarantee NY_Bangalore
```

## Flow (`01-MinimumGuaranteeCampaignFlow.json`)

| Folder | What it proves |
|---|---|
| 0 Driver setup | OTP login, switch city, add AUTO vehicle, enable driver (same steps as IncentiveJourneyFlow) |
| 1 Campaign validation | no windows, overlapping windows, first slab not at 0, duplicate metric, guarantee ≤ 0 are all rejected with 400 |
| 2 Draft | create → listed as `DRAFT` with `createdBy`; list `limit` above 50 is clamped; DRAFT terms editable (guarantee 2000 → 2500); invite is idempotent (second call skips); a DRAFT campaign is hidden from the driver and cannot be opted into |
| 3 Publish | `DRAFT → PUBLISHED`; terms locked afterwards; re-publish refused |
| 4 Driver opt-in | invitation visible with updated terms and `canOptIn` for tomorrow; opt-in needs `acceptTerms`; opt-in is idempotent; progress shows `UPCOMING` with zero counters; admin enrollments list shows the driver; opt-out → `OPTED_OUT`; re-opt-in → `UPCOMING` |
| 5 Pause, extend, cancel | paused campaign refuses opt-in; resume; extend only moves the end date later; with the campaign extended, opting in for the day after tomorrow is still refused (only today and tomorrow are open); cancel calls off upcoming days (`CANCELLED_BY_ADMIN`); a cancelled campaign cannot be re-published or opted into |
| 6 Budget cap | `maxBudget` (100) below one day's guarantee (2000): opt-in is refused with "fully subscribed" |

## Dates

Opt-in is open for today and tomorrow, and refused after `first window start − optInCutoffMinutes`. Windows are 08:00–10:00 and 17:00–19:00 IST, so the flow opts in for **tomorrow** (IST), which is always before the cutoff. The collection computes `_test_mg_today`, `_test_mg_tomorrow` and `_test_mg_day_after` in its pre-request script.

## Not covered (needs a clock the collection cannot move)

- **Settlement and top-up.** The per-campaign `MinimumGuaranteeSettlement` job first runs 15 minutes after publish and settles days whose last window ended plus the grace; the ledger credit needs a day that has already happened. Covered by `hunit-tests` (`MinimumGuaranteeRulesTests`: slab, break, due-day and top-up arithmetic).
- **Ride, cancellation and mode-change events** inside a window (counters, disqualification, force-assign): they need a ride inside a live window.
- **Fleet owner opt-in** (`…/fleet/minimumGuarantee/*`): needs a fleet owner with associated drivers; its capability SQL is seeded already.
