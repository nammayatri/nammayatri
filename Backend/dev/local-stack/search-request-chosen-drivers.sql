-- Give search_request somewhere to keep the drivers the passenger picked.
--
-- ── What this is for ────────────────────────────────────────────────────────
-- Until now every driver the pool found was asked, in batches, and the first to
-- answer won. The client asked for the other thing: the passenger sees the cars
-- near him and sends the request to the one, two or three he wants.
--
-- The route it travels already existed. `select` has always carried one rider
-- decision to the provider — `auto_assign_enabled`, a boolean riding in
-- order.fulfillment.tags, stored on this same table and read back by the
-- allocator. The shortlist rides in the same tags, into the column below, and
-- is read at the same moment: one filter on the pool, in
-- prepareDriverPoolBatch, before the batching and the sorting and the radius
-- expansion all of which work off that one list.
--
-- ── Apply this BEFORE swapping the image ────────────────────────────────────
--   1. run this. The deployed binary does not know the column and does not
--      care — Postgres fills NULL for a nullable column nobody mentions, and
--      every insert keeps working.
--   2. swap to the new image.
--
-- Rollback is then a plain image swap. Do not drop the column on rollback: the
-- old binary tolerates it exactly as it did in step 1, and dropping it is the
-- only way to turn a reversible deploy into an irreversible one.
--
-- Idempotent: safe to re-run, before or after the deploy.

\set ON_ERROR_STOP on

-- Comma-separated person ids. NULL means the passenger did not choose, and is
-- the ordinary case — every existing row, and every future row from a rider who
-- pressed nothing. The allocator returns the pool whole for NULL, so the
-- default behaviour is the old behaviour and costs no extra work.
ALTER TABLE atlas_driver_offer_bpp.search_request
  ADD COLUMN IF NOT EXISTS chosen_drivers text;

COMMENT ON COLUMN atlas_driver_offer_bpp.search_request.chosen_drivers IS
  'Algeria: comma-separated person ids the passenger picked. NULL = ask every driver in the pool.';

-- How often anyone actually chooses. All-NULL on the day of the migration is
-- correct, not a failure: nothing writes here until an APK carrying the picking
-- screen is installed.
SELECT
  count(*)                                    AS searches,
  count(chosen_drivers)                       AS with_shortlist,
  count(*) - count(chosen_drivers)            AS asked_everyone
FROM atlas_driver_offer_bpp.search_request
WHERE created_at > now() - interval '7 days';
