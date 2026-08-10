# Dashboard unification — merge runbook

One-time, manually-run migration that merges the BAP and BPP dashboard data into
the **`atlas_dashboard`** database (schema `atlas_dashboard`, whose tables
already exist).

The three databases are separate, so both sides are exported to CSV and loaded
into `atlas_dashboard` as **prefixed staging tables** (`bap_*`, `bpp_*`). The
merge reads those and writes the real tables in the same schema. The source
databases are never written to, so rollback is "don't flip the configs".

> The commands below use the internal DB host/user for convenience. This repo is
> public — replace them with placeholders before pushing if that matters.

```bash
export PGHOST=10.60.102.38 PGPORT=5432 PGUSER=atlas_rw
mkdir -p /tmp/unify && cd /tmp/unify
R=<repo>/Backend/dev/seed-migrations/dashboard-unification

# 1. export both sides (CSVs land in the current directory)
psql atlas_bap_dashboard -f $R/000a-export-bap.sql
psql atlas_bpp_dashboard -f $R/000b-export-bpp.sql

# 2. stage them into the target
psql -v ON_ERROR_STOP=1 atlas_dashboard -f $R/000c-import.sql   # prints row counts

# 3. merge — check each gate before continuing
for f in 0000 0001 0002 0003 0004 0005 0006 0007 0008 0009 0010 0011 0012 0013 0016 0017; do
  psql atlas_dashboard -f $R/$f-*.sql
done
```

Run every psql from the same directory: filenames inside the scripts are literal
and relative, because psql does not interpolate variables inside `\copy`.

## Files

| File | What | Gate before continuing |
|---|---|---|
| `000a-export-bap.sql` | **on `atlas_bap_dashboard`** — dump its tables to CSV | — |
| `000b-export-bpp.sql` | **on `atlas_bpp_dashboard`** — same | — |
| `000c-import.sql` | **on `atlas_dashboard`** — create `bap_*`/`bpp_*` staging, load CSVs | row counts match the sources |
| `000d-hotfix-merchant-operating-city.sql` | standalone repair for a target schema built without `merchant_operating_city`; no-op after a full run | table non-empty |
| `0000-phase0-parity.sql` | person/password/role parity across the two sides | reviewed |
| `0001-preflight.sql` | staging present, id/key collisions, retired-role members | every section empty or explained |
| `0002-schema.sql` | asserts the target tables exist; creates `legacy_bap_person`, `merchant_pair` | no exception raised |
| `0003-capability-ddl.sql` | capability tables (skip if already present) | — |
| `0004-roles.sql` | role union, BPP ids canonical, zero-member prune | 2.4 counts match |
| `0005-persons.sql` | BPP copy, match map, BAP-only copy, admin_tier carried across | 3.4 returns 0 |
| `0006-merchants.sql` | merchant union + `merchant_pair` seed | 4.4 returns 0 |
| `0007-access.sql` | merchant_access + access_matrix unions, stale-grant purge | — |
| `0008-transactions-entities.sql` | audit log, entities, access_audit; idempotent → re-run as cutover delta | — |
| `0009-capability-seed.sql` | capability catalog, endpoint shim, role bundles (GENERATED) | — |
| `0010-split-role-overrides.sql` | carries existing person_capability overrides, adds split-role grants | 7.2 reviewed; 7.3 sensitive-loss list dispositioned |
| `0011-verdict-diff.sql` | legacy vs capability disagreement matrix | breakage section empty/explained |
| `0012-verify.sql` | counts, integrity, password policy, tier distribution; staging cleanup | all assertions at expected values |
| `0013-curation-worksheet.sql` | widening review for roles with >5 members | reviewed |
| `0016-capability-endpoint-gap.sql` | **runs first** — capability_endpoint rows for the 31 endpoints that shipped after the 2026-08-05 inventory | gate returns 0 |
| `0017-capability-backfill.sql` | **required before the no-fallback binary** — grants every role the capability behind each endpoint the matrix already allowed it | §1 empty, §6 both 0 |
| `0018-super-admin-seed.sql` | **required in the same window** — seeds the initial SUPER_ADMIN(s) by person id | §5 lists exactly who you intended |
| `0019-pt-employee-persons.sql` | restores the two PT-employee logins: carries `token_no_hash`/`entity_id` onto a phone-matched person, or inserts them outright | §6 shows 2 rows, `can_pt_login` true |

`0003` and `0009` are generated — source of truth is
`docs/access-unification/generate_capability_seed.py` in the control-center repo.
Regenerate there and re-substitute; never hand-edit.

## What is and is not migrated

- **Carried across:** roles, persons (BPP row canonical, BPP password wins),
  merchants, `merchant_operating_city`, merchant/city access, the access matrix, entities, audit history,
  `person_capability` overrides, and `person.admin_tier`.
- **Regenerated, not copied:** `capability`, `capability_endpoint`,
  `role_capability` — 0009 rebuilds them from the UNION of both access matrices,
  which is better than copying per-side derivations.
- **Never migrated:** `registration_token`. Cutover forces re-login, which is
  also the token-invalidation story.
- **Optional:** the `transaction` and `entity` lines are commented in the export
  and import scripts — uncomment on the sides that have them.

The CSVs contain encrypted PII and identity hashes: keep them off shared disks
and delete them after the import.

## Enforcement cutover (capability becomes the only authority)

Authorization no longer consults `access_matrix` — not in the dashboard
(`Tools.Auth.Capability.enforce`) and not in the control-center Express server
(`config/capabilityMap.ts`, now default-deny). Both fail closed on an endpoint
with no capability behind it. Order matters:

1. `0016-capability-endpoint-gap.sql`. 31 endpoints shipped after the inventory
   `0009` was generated from, so they have an `access_matrix` row and no
   capability at all — every one of them 403s the moment the fallback goes.
2. `0017-capability-backfill.sql`. Until this runs the capability set is a
   *subset* of the matrix — 0009 derives a capability only at >= 50%% endpoint
   coverage — so deploying first turns every sub-threshold holding into a 403.
   Its §1 report must be empty and §6 must read 0/0.
3. `0018-super-admin-seed.sql`. `guardAdminMutation` is no longer
   existence-guarded, so with no SUPER_ADMIN row nobody can create or modify a
   DASHBOARD_ADMIN.
4. Deploy the dashboard binary, then the Express server.
5. Watch `CAPABILITY_DENIED`, `CAPABILITY_UNMAPPED_ENDPOINT` (dashboard) and
   `CAPABILITY_UNMAPPED_ROUTE` (Express). Unmapped lines are seeding/mapping
   bugs and each one is somebody's 403 — fix by extending the seed generator or
   `capabilityMap.ts`, not by re-adding a fallback.

Backfill widens: capability is a coarser unit than the matrix, so a role
holding 1 of a capability's 10 endpoints now gets all 10. That is the
deliberate trade (PLAN.md: "give access what they have, we will limit later").
§3 quantifies it and §4 is the trimming worklist.

## Cutover (per environment: integ → prod → EU)

1. Re-run `000a`/`000b`/`000c` and `0008` for a delta of rows written since the
   bulk load.
2. Confirm every table the dashboards read exists in the target — `0002` asserts
   this. A missing one only surfaces at runtime as `relation "..." does not
   exist`; that is how `merchant_operating_city` was missed once.
3. Point both dashboards at the merged store — config only, no rebuild
   (`Storage.Beam.SchemaName` reads it at startup):
   `connectDatabase = "atlas_dashboard"`, `connectSchemaName = "atlas_dashboard"`.
4. Unify the Redis auth prefix and flush the two old namespaces. All users
   re-log-in.
5. Watch: login success rate, `CAPABILITY_SHADOW_*` volume, 5xx on `/bap/*` and
   `/bpp/*`.

Rollback: point the dhall configs back. The source databases were never written
to, so pre-cutover state is intact.

## Decisions encoded here

See `docs/access-unification/PLAN.md` (control-center repo) for the full
decisions log — password policy, split-role handling, retired roles, capability
seeding, and the `capability OR legacy` flip semantics.
