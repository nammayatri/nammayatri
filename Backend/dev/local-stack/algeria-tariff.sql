-- Movin DZ — the Algerian tariff.
--
-- Set by the client on 2026-08-13, replacing the upstream project's seeded
-- numbers (base 10, 12 per km, 120 pickup) which were designed around a
-- Bangalore auto-rickshaw and made every vehicle cost the same 258 DZD.
--
--   Category   Variant      Start   Per km   Pickup
--   Economy    HATCHBACK      100       35       50
--   Comfort    SEDAN          150       45       70
--   Premium    SUV            200       60      100
--
-- Driver's maximum extra: 300 DZD, flat, all categories.
--
-- ── Three things this has to get right, none of them obvious ────────────────
--
-- 1. THERE ARE TWO MERCHANTS. `favorit0-…` (NAMMA_YATRI_PARTNER) and
--    `nearest-drivers-testing-organization` (OTHER_MERCHANT_2), with 6 and 7
--    seeded drivers respectively. Both dispatch. A tariff applied to one leaves
--    half the fleet quoting the old price, so these statements are deliberately
--    not filtered by merchant.
--
-- 2. `restricted_extra_fare` OVERRIDES `fare_policy.driver_max_extra_fee`.
--    Measured: fare_policy said 30, the app showed a range of +20, and
--    restricted_extra_fare said 20 for trips over 3 km. Setting only the
--    obvious column changes nothing a rider can see.
--
-- 3. `base_distance_meters` IS SET TO 0, so the per-km charge applies from the
--    first metre. The client wrote "starting price" and "price per km" without
--    saying whether the start includes any distance; the seed had it covering
--    the first 3 km. Taken literally as a flat start plus distance — which is
--    the reading that makes his 100/35/50 arithmetic come out at ordinary
--    Algiers prices. **If he meant the start to include the first 3 km, this is
--    the one line to change**, and every fare drops by about 100 DZD.
--
-- Idempotent: re-running sets the same values.

BEGIN;

-- ── Economy ────────────────────────────────────────────────────────────────
UPDATE atlas_driver_offer_bpp.fare_policy
   SET base_distance_fare   = 100,
       base_distance_meters = 0,
       per_extra_km_fare    = 35,
       dead_km_fare         = 50,
       driver_min_extra_fee = 0,
       driver_max_extra_fee = 300,
       updated_at           = now()
 WHERE vehicle_variant = 'HATCHBACK';

-- ── Comfort ────────────────────────────────────────────────────────────────
UPDATE atlas_driver_offer_bpp.fare_policy
   SET base_distance_fare   = 150,
       base_distance_meters = 0,
       per_extra_km_fare    = 45,
       dead_km_fare         = 70,
       driver_min_extra_fee = 0,
       driver_max_extra_fee = 300,
       updated_at           = now()
 WHERE vehicle_variant = 'SEDAN';

-- ── Premium ────────────────────────────────────────────────────────────────
UPDATE atlas_driver_offer_bpp.fare_policy
   SET base_distance_fare   = 200,
       base_distance_meters = 0,
       per_extra_km_fare    = 60,
       dead_km_fare         = 100,
       driver_min_extra_fee = 0,
       driver_max_extra_fee = 300,
       updated_at           = now()
 WHERE vehicle_variant = 'SUV';

-- ── The auto-rickshaw ──────────────────────────────────────────────────────
-- Two thirds of the seeded fleet and a vehicle nobody in Algiers hails, so the
-- app filters it out of the price list. Priced as Economy anyway: if it is ever
-- unfiltered it must not be the one row still quoting Bangalore.
UPDATE atlas_driver_offer_bpp.fare_policy
   SET base_distance_fare   = 100,
       base_distance_meters = 0,
       per_extra_km_fare    = 35,
       dead_km_fare         = 50,
       driver_min_extra_fee = 0,
       driver_max_extra_fee = 300,
       updated_at           = now()
 WHERE vehicle_variant = 'AUTO_RICKSHAW';

-- ── The cap the backend actually obeys ─────────────────────────────────────
-- Flat 300 at every distance, per the client on 2026-08-13. He also said the
-- extra should ideally be at most 50% of the fare, which this table could
-- express — it is keyed on `min_trip_distance` precisely so the cap can grow
-- with the trip — but he chose the flat number for now. The bands are left in
-- place so that switching to the percentage rule later is an UPDATE, not a
-- schema change.
UPDATE atlas_driver_offer_bpp.restricted_extra_fare
   SET driver_max_extra_fare = 300;

COMMIT;

-- ── What this produces, for checking against expectations ──────────────────
SELECT fp.vehicle_variant,
       CASE fp.vehicle_variant
         WHEN 'HATCHBACK' THEN 'Economy'
         WHEN 'SEDAN'     THEN 'Comfort'
         WHEN 'SUV'       THEN 'Premium'
         ELSE '(hidden)'
       END                                            AS category,
       fp.base_distance_fare                          AS start,
       fp.per_extra_km_fare                           AS per_km,
       fp.dead_km_fare                                AS pickup,
       fp.driver_max_extra_fee                        AS max_extra,
       (fp.base_distance_fare + 3  * fp.per_extra_km_fare + fp.dead_km_fare)  AS trip_3km,
       (fp.base_distance_fare + 14 * fp.per_extra_km_fare + fp.dead_km_fare)  AS trip_14km
  FROM atlas_driver_offer_bpp.fare_policy fp
 GROUP BY 1,2,3,4,5,6,7,8
 ORDER BY fp.base_distance_fare;
