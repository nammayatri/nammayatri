# PT multi-entity — frontend migration guide

**Backend is safe to release first.** Every change in this PR is additive on the wire: no field
was removed, no field changed type, no route was renamed away, and no existing response changed
shape. The frontend can be released at any time afterwards.

This document lists what the frontend should **start reading**, and what it should **stop
reading** so the deprecated fields can be deleted in a later PR.

---

## 1. What changed conceptually

A PT employee (conductor / depot manager) used to be assigned to exactly **one** entity (depot),
stored as a single `person.entity_id` column. They can now hold **many**.

Grants moved out of the `person` row into a new `entity_access` table, one row per
`(person, entity)`. The old single-entity fields are still populated — they now report the
**first** entity of the list — so nothing breaks while the frontend catches up.

`tokenNo` and `vpa` are also now stored encrypted and can be read back, which is what makes the
new PT list endpoint possible.

---

## 2. Field-by-field migration

### `GET /user/profile` → `PersonAPIEntity`

| Field | Status | Action |
| --- | --- | --- |
| `entityId` | **Deprecated** — now the first entity's id | Stop reading. Replace with `entityShortIds`. |
| `entityName` | **Deprecated** — now the first entity's name | Stop reading. Replace with `entityShortIds`. |
| `entityShortIds` | **New** — `["CENTRAL", "CENTRAL_EV"]` | Start reading. This is the full depot list. |
| `tokenNo` | **New** — the person's own token, decrypted | Optional. Only populated here, never on `/admin/person/list`. |

A person with no depots returns `entityShortIds: []` and `entityId: null` / `entityName: null`,
exactly as before.

### `POST /.../login` → `LoginRes`

| Field | Status | Action |
| --- | --- | --- |
| `entityName` | **Deprecated** — now the first entity's name | Stop reading. Replace with `entityShortIds`. |
| `entityShortIds` | **New** | Start reading. Drives the PT welcome screen when a user covers several depots. |

### `GET /admin/person/list` → `ListPersonRes`

**Unchanged.** Deliberately untouched — same tenancy behaviour, same pagination, same payload.
Two new fields appear in each row and are always inert here:

- `entityShortIds` — always `[]`
- `tokenNo` — always `null`

Do not start reading either from this endpoint. Use `/admin/person/ptList` (below) for anything
PT-related.

### `POST /.../person/bulkCreate` → renamed to `bulkUpsert`

| Item | Status | Action |
| --- | --- | --- |
| `POST /person/bulkCreate` | **Deprecated alias** — still live, identical behaviour | Switch the path to `/person/bulkUpsert`. |
| `POST /person/bulkUpsert` | **New canonical path** | Use this. |
| request field `entityId` | **Deprecated** — still accepted and merged | Stop sending. Send `entityIds` instead. |
| request field `entityIds` | **New** — `["CENTRAL_UUID", "EV_UUID"]` | Start sending. |

The request and response JSON are otherwise unchanged, so switching the path alone is a safe
first step.

**`entityIds` is three-state** — this is the part to get right:

| `entityIds` | Effect on that person's depots, **under this merchant only** |
| --- | --- |
| omitted | untouched |
| `[]` | every grant revoked |
| `["A", "B"]` | set to exactly A and B |

It is a **replace**, not an append. To add a depot to someone who already has one, send the full
list. Read the current list from `/admin/person/ptList` first.

Sending only the legacy `entityId` behaves exactly as it did before — with one depot, "replace
with this one" and "set this one" are the same operation.

> The values here are entity **UUIDs**, not short ids. `/admin/person/ptList` returns short ids.
> Use `GET /entity/list` to map between them, or ask backend to accept `entityShortIds` on the
> request too.

---

## 3. New endpoint: `GET /admin/person/ptList`

Purpose-built for depot-operations screens. `DASHBOARD_ADMIN`, scoped to the caller's merchant.

```
GET /admin/person/ptList
      ?searchString=      optional — name (first+last) or exact mobile number
      &roleName=          optional — e.g. PT_CONDUCTOR, PT_DEPOT_MANAGER
      &entityShortId=     optional — only staff granted that depot
      &limit=&offset=     optional
```

All four filters are optional and combine.

```json
{
  "list": [
    {
      "id": "…",
      "firstName": "Ravi",
      "lastName": "Kumar",
      "mobileNumber": "9999999999",
      "mobileCountryCode": "+91",
      "email": null,
      "roleName": "PT_DEPOT_MANAGER",
      "tokenNo": "40231",
      "vpa": "ravi@okaxis",
      "entityShortIds": ["CENTRAL", "CENTRAL_EV"],
      "verified": true,
      "registeredAt": "2026-08-17T11:47:38Z"
    }
  ],
  "summary": { "totalCount": 1, "count": 1 }
}
```

Notes:

- The base set is **everyone with a `tokenNo`**. Staff provisioned before their token is issued
  will not appear here.
- `vpa` is exposed **only** on this endpoint.
- An unknown or another merchant's `entityShortId` returns `400 InvalidRequest`, not an empty
  list. A misspelled `roleName` returns an empty list (it is not validated).
- `summary.totalCount` is a real `COUNT(*)` over all matching rows, ignoring `limit`/`offset`,
  so it can drive a page count. `summary.count` is the size of this page. Note
  `/admin/person/list` is different — it returns a hardcoded `10000` for `totalCount` — so the
  two endpoints do **not** agree on what that field means. Do not share pagination logic across
  them.

---

## 4. Frontend checklist

Ordered so each step is independently shippable.

- [ ] **Read `entityShortIds`** on `/user/profile` and on `LoginRes`; render the full depot list
      instead of a single name.
- [ ] **Switch the bulk path** from `/person/bulkCreate` to `/person/bulkUpsert`.
- [ ] **Send `entityIds`** instead of `entityId` in the bulk payload. Remember it replaces —
      always send the complete desired list.
- [ ] **Build the PT screen** on `/admin/person/ptList` rather than filtering
      `/admin/person/list` client-side.
- [ ] **Stop reading `entityId` / `entityName`** anywhere.

Once all of the above are live in every client, backend can delete:

- `PersonAPIEntity.entityId`
- `PersonAPIEntity.entityName`
- `LoginRes.entityName`
- `BulkUpsertPerson.entityId` (request field)
- the `POST /person/bulkCreate` route alias
- the `person.entity_id` **column** (see below)

---

## 5. Backend-side deploy notes

Not frontend work, but relevant to release sequencing.

**Migrations must be applied before the binary ships.** The new binary `SELECT`s `entity_access`;
every profile and login read fails without the table.

DDL, in `dev/ddl-migrations/`:

| Schema | `entity_access` table | vpa / encrypted-token columns |
| --- | --- | --- |
| `atlas_dashboard` | `provider-dashboard/0106-create-entity-access.sql` | `provider-dashboard/0107` |
| `atlas_bap_dashboard` | `rider-dashboard/0077-create-entity-access.sql` | `rider-dashboard/0076` |
| `atlas_safety_dashboard` | `safety-dashboard/0013-create-entity-access.sql` | `safety-dashboard/0014` |

Then the backfill, in `dev/seed-migrations/` — **run after the DDL, before the binary serves
traffic**. Until it runs, every existing depot manager reads as having no depot:

- `provider-dashboard/0005-backfill-person-entity-access.sql`
- `rider-dashboard/0006-backfill-person-entity-access.sql`

Safety-dashboard has no `entity` table and no PT flow, so it gets the table (to satisfy the
shared Beam binding) but no backfill.

**`person.entity_id` is retained and is the rollback path.** The migration backfills
`entity_access` from it; the new binary then neither reads nor writes it. Two consequences:

- *Rolling deploy:* an old pod still reads `person.entity_id`, so it shows the pre-deploy depot.
  Correct for anyone not edited during the rollout window; self-heals as pods cycle.
- *Rollback:* reverting the binary restores the pre-deploy assignment view. Depot changes made
  while the new binary ran survive in `entity_access` and reappear on roll-forward — they are not
  lost, just invisible to the old binary.

Drop the column only once the release has soaked and rollback is off the table.
