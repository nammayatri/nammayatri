# dashboard-unification — Phase 1 copy-merge runbook

One-time, manually-run (psql) migration building the merged `atlas_dashboard`
schema from `atlas_bap_dashboard` + `atlas_bpp_dashboard`. Part of the
dashboard-unification project; the design doc of record is
`docs/access-unification/PLAN.md` in the control-center repo.

**Copy-only invariant:** source schemas are never written to. Rollback at any
stage = don't flip the servers (optionally `DROP SCHEMA atlas_dashboard
CASCADE`). Nothing in this folder is executed by dev tooling or CI — numbered
for run order only.

## Files

| File | What | Gate before continuing |
|---|---|---|
| `0000-phase0-parity.sql` | read-only analysis: person/password/role parity across sides | reviewed |
| `0001-preflight.sql` | column drift, id/key collisions, retired-role members | every section empty or explained |
| `0002-schema.sql` | schema + `LIKE`-cloned table shells + FKs + `legacy_bap_person` + `merchant_pair` | — |
| `0003-capability-ddl.sql` | capability-framework tables (GENERATED — see below) | — |
| `0004-roles.sql` | role union, BPP ids canonical, zero-member prune, `accessible_roles` scrub | 2.4 counts match |
| `0005-persons.sql` | BPP copy (INTERNAL_ADMIN→JUSPAY_ADMIN), match map, BAP-only copy, admin_tier init | 3.4 returns 0 |
| `0006-merchants.sql` | merchant union + `merchant_pair` seed | 4.4 returns 0 |
| `0007-access.sql` | merchant_access + access_matrix unions, stale-grant purge | — |
| `0008-transactions-entities.sql` | audit log + entities; idempotent → re-run as cutover delta | — |
| `0009-capability-seed.sql` | capability catalog + endpoint shim + role bundles (GENERATED) | — |
| `0010-split-role-overrides.sql` | person_capability GRANTs for split-role persons + review reports | 7.2 reviewed; 7.3 sensitive-loss list dispositioned |
| `0011-verdict-diff.sql` | legacy vs capability disagreement matrix — primary pre-flip gate | breakage section empty/explained |
| `0012-verify.sql` | counts, integrity, password policy, tier distribution | all assertions at expected values |

`0003` and `0009` are generated: source of truth is
`docs/access-unification/generate_capability_seed.py` (control-center repo),
whose outputs also produced `dev/ddl-migrations/{provider,rider}-dashboard`
`0091/0062-capability-framework.sql` and the per-side capability seeds. To
regenerate: run the generator, then `sed 's/{schema}/atlas_dashboard/g'` its
`sql/` outputs into these two files. Do not hand-edit.

## Cutover (per environment: integ → prod → EU)

**The schema binding is COMPILE-TIME, not only config**: each app hardcodes
its schema in `src/Storage/Beam/CommonInstances.hs` (`HasSchemaName`
instances). Cutover therefore requires deploying binaries built with those
instances saying `atlas_dashboard`, alongside the dhall change.

1. Re-run `0008` (delta copy of audit rows written since the bulk run).
2. Point BOTH dashboard servers at the merged schema:
   - `src/Storage/Beam/CommonInstances.hs` in each app: every
     `schemaName` -> `"atlas_dashboard"` (one-line-per-instance code change,
     built ahead of the window);
   - `esqDBCfg.connectSchemaName = "atlas_dashboard"` in each dhall config.
3. Unify the Redis auth namespace to one `authTokenCacheKey` prefix
   (`dashboard:`); flush old `rider-dashboard:authTokenCacheKey:*` and
   `provider-dashboard:authTokenCacheKey:*` keys.
4. Deploy/restart both servers. All users re-log-in (`registration_token` is
   deliberately not migrated — that IS the token invalidation).
5. Watch: login success rate, `CAPABILITY_SHADOW_*` log volume, 5xx on
   `/bap/*` and `/bpp/*`.

**Ordering constraint with Phase 2 (server merge):** the provider-dashboard
binary now also mounts the `/bap/*` tree. That code can DEPLOY any time (it
is inert without traffic), but the FRONTEND may only be pointed at the single
server AFTER this schema cutover — before it, `/bap/*` on provider-dashboard
would resolve persons/tokens against `atlas_bpp_dashboard`, where BAP users
don't exist (and the `entity` table isn't present). Sequence per env:
schema cutover (above) → flip `VITE_API_URL`/`VITE_BAP_URL` to the merged
server → bake with rider-dashboard as fallback → decommission rider-dashboard.

Rollback post-cutover: revert the dhall values + Redis prefixes, redeploy.
Only loss: audit rows written into `atlas_dashboard` during the flipped
window — export before dropping the schema if abandoning.

## Decisions encoded here (see PLAN.md decisions log for full context)

- BPP person row + password hash canonical for both-sides users (no reset
  campaign); BAP ids traceable via `legacy_bap_person`.
- INTERNAL_ADMIN retired → members become JUSPAY_ADMIN. CUSTOMER/DRIVER and
  all zero-member roles dropped (except FINANCE_ADMIN, SlaMonitoring).
- Split-role persons: BPP role stays primary; BAP-bundle delta arrives as
  person_capability GRANTs (`merge 2026:` reason). Sensitive capabilities
  never auto-granted — 0010's 7.3 report lists them for human disposition.
- SUPER_ADMIN is NOT seeded here: manual vault-controlled INSERT only.
