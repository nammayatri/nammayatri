-- Give rider_details somewhere to keep what drivers made of a passenger.
--
-- ── What this is one half of ────────────────────────────────────────────────
-- The client asked for drivers to rate passengers, and it was refused three
-- times for a reason that was true at the time: **this backend could not do
-- it.** The only rating route in the whole driver API is
-- `/beckn/{merchantId}/rating`, which is the provider *receiving* a rating from
-- the rider app over BECKN, and `atlas_driver_offer_bpp.rider_details` had five
-- columns — id, country code, encrypted number, two timestamps — with nowhere
-- to put one. That is why D18 ships a star that points one way and deliberately
-- no "Noter" pill: a control that cannot do anything is worse than an absent
-- one, and this project has already shipped two of those.
--
-- These columns are the other half. The route that writes them is
-- `POST /ui/driver/ride/{rideId}/rateCustomer`, added by apply-patches.py.
--
-- ── Why three columns and not one ───────────────────────────────────────────
-- A driver's own average is rebuilt from scratch by reading every row in the
-- `rating` table — see `calculateAverageRating`. Passengers have no such table
-- and are not getting one, so there is nothing to recompute an average *from*.
-- Keeping the count and the running sum alongside it makes the next average one
-- addition instead of a table scan, and it means the average can never drift
-- away from the ratings that produced it.
--
-- `total_ratings` and `total_rating_score` are NOT NULL with a default because
-- the Haskell reads them as `Int`, not `Maybe Int` — a null in either would be
-- a decode failure on every ride list, not a missing star.
--
-- ── Apply this BEFORE swapping the image ────────────────────────────────────
-- The order matters and it is the safe order, not the risky one:
--
--   1. run this. The currently deployed binary does not know these columns and
--      does not care — it never names them, and the defaults satisfy the two
--      that cannot be null.
--   2. swap to the new image.
--
-- Rollback is therefore a plain image swap with nothing to undo here. Do not
-- drop these on rollback — dropping them is the only way to turn a reversible
-- deploy into an irreversible one, and it would take every rating with it.
--
-- Idempotent: safe to re-run, and safe to run before or after the deploy.

\set ON_ERROR_STOP on

ALTER TABLE atlas_driver_offer_bpp.rider_details
  ADD COLUMN IF NOT EXISTS rating             double precision,
  ADD COLUMN IF NOT EXISTS total_ratings      integer NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS total_rating_score integer NOT NULL DEFAULT 0;

COMMENT ON COLUMN atlas_driver_offer_bpp.rider_details.rating IS
  'Algeria: average of what drivers gave this passenger, 1-5. NULL until somebody rates.';
COMMENT ON COLUMN atlas_driver_offer_bpp.rider_details.total_ratings IS
  'Algeria: how many drivers have rated. Kept so the average needs no table scan.';
COMMENT ON COLUMN atlas_driver_offer_bpp.rider_details.total_rating_score IS
  'Algeria: sum of those ratings. average = score / count, to the dinar of arithmetic.';

-- What the deploy should look like afterwards. Nothing is written until a
-- driver rates somebody on the new image, so an all-NULL rating column here is
-- the correct state on the day of the migration, not a failure.
SELECT
  count(*)                       AS passengers,
  count(rating)                  AS rated,
  count(*) - count(rating)       AS not_yet_rated
FROM atlas_driver_offer_bpp.rider_details;
