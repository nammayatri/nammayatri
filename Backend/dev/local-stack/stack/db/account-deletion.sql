-- Movin DZ — account deletion requests.
--
-- Google Play requires an in-app path to request deletion, and the deployed
-- backend has no deletion route at all: `deleteAccount`, `deleteProfile` and
-- `account/delete` are absent from both running binaries, checked inside the
-- container. So a request is RECORDED here and a person carries it out from
-- the admin site — which is exactly what the app's screen 23 says, and why it
-- says *demande enregistrée* rather than *compte supprimé*.
--
-- ── Why this is its own table and not a column on `person` ──────────────────
-- `person` lives in the upstream schemas, which their migrations own. Adding a
-- column there is a change they can silently undo. `movin` is ours, the same
-- reasoning as `movin.subscription`, and dropping the whole schema removes the
-- feature cleanly if it is ever replaced by a real backend route.
--
-- ── No foreign key, deliberately ────────────────────────────────────────────
-- Same rule as the subscription tables: a foreign key from our schema into
-- theirs can block their migrations, and the id is validated by the shim from
-- the caller's token before it ever reaches here.
--
-- ── One open request per account, not per row ───────────────────────────────
-- The partial unique index is the whole concurrency story: two taps on a slow
-- connection cannot both insert, so the app's "already requested" state is a
-- fact rather than a race. Withdrawn and completed requests stay as history —
-- the index only constrains what is still open.
--
-- Idempotent: re-running changes nothing.

BEGIN;

CREATE SCHEMA IF NOT EXISTS movin;

CREATE TABLE IF NOT EXISTS movin.deletion_request (
  id            bigserial PRIMARY KEY,
  -- 'rider' or 'driver'. The two are separate accounts on separate backends
  -- and one phone may hold both, so deleting one must never take the other.
  side          text        NOT NULL CHECK (side IN ('rider', 'driver')),
  -- The person id, resolved by the shim from the caller's token. Never sent
  -- by the client: a request shape that carried an id would be a way to
  -- delete somebody else's account.
  person_id     text        NOT NULL,
  -- Kept so the office can match a request to a caller who rings up about it.
  -- It is the number the account signs in with, which is already in `person`.
  phone         text,
  reason        text,
  requested_at  timestamptz NOT NULL DEFAULT now(),
  -- What the app promised on screen 23. Stored rather than computed at read
  -- time so the date shown to the person never moves afterwards.
  delete_by     timestamptz NOT NULL DEFAULT now() + interval '30 days',
  -- open → withdrawn (the person changed their mind)
  --      → done      (an administrator carried it out)
  status        text        NOT NULL DEFAULT 'open'
                  CHECK (status IN ('open', 'withdrawn', 'done')),
  handled_at    timestamptz,
  handled_by    text,
  note          text
);

CREATE UNIQUE INDEX IF NOT EXISTS deletion_request_one_open
  ON movin.deletion_request (side, person_id)
  WHERE status = 'open';

CREATE INDEX IF NOT EXISTS deletion_request_queue
  ON movin.deletion_request (status, delete_by);

-- What the admin site lists. A view rather than a query in the site, so the
-- definition of "the queue" lives next to the table it reads.
CREATE OR REPLACE VIEW movin.deletion_queue AS
SELECT r.id,
       r.side,
       r.person_id,
       r.phone,
       r.reason,
       r.requested_at,
       r.delete_by,
       (r.delete_by < now()) AS overdue,
       greatest(0, date_part('day', r.delete_by - now()))::int AS days_left
  FROM movin.deletion_request r
 WHERE r.status = 'open'
 ORDER BY r.delete_by;

COMMIT;
