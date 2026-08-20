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
-- Driver's maximum extra: **grows with distance, ~50% of the fare**. See the
-- band table at the bottom. A flat cap was tried first and rejected — 300 is
-- 44% of a long ride and 118% of a short one.
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

-- ── The cap the backend actually obeys, growing with distance ──────────────
--
-- The client's rule, agreed 2026-08-13: the driver's extra should be **at most
-- half the fare**. A flat cap cannot do that — 300 is 44% of a 15 km Economy
-- ride and 118% of a 3 km one, and short trips are most trips.
--
-- This table is keyed on `min_trip_distance` precisely so the cap can grow, so
-- the rule is expressible without touching the backend.
--
-- ── THE BANDS ARE THE SAME FOR EVERY CATEGORY, AND THAT IS NOT LAZINESS ────
-- Per-category bands were tried first and **the backend does not honour them**.
-- Measured with three searches after loading Economy/Comfort/Premium caps of
-- 100/125/150, 180/245/330 and 250/335/450:
--
--   1.6 km   Economy, Comfort and Premium ALL came back +125
--   7.4 km   all three came back +330
--  13.7 km   all three came back +450
--
-- Identical across categories within one search, and a different variant's row
-- each time — so the cap is resolved once per search rather than per estimate,
-- and which row wins is not something to rely on.
--
-- The consequence is the whole design of this table: **the cap must be sized
-- against the CHEAPEST category**, because whatever is chosen applies to all
-- three. Economy is the cheapest, so each band is 50% of the *Economy* fare at
-- the band's lower bound. Comfort and Premium then sit comfortably under half,
-- which is the right way round — the error is always in the rider's favour.
--
--   Economy = 150 + 35/km   (start 100 + pickup 50, then distance)
--
-- The bands are close together on purpose. A cap only steps up at a boundary
-- while the fare rises continuously, so wide bands drift well below 50% before
-- catching up; these track it within a few percent.
--
-- Rebuilt rather than updated, because the number of bands changes. Both
-- merchants get rows: without them a merchant falls back to
-- `fare_policy.driver_max_extra_fee`, which would leave half the fleet on a
-- flat cap while the other half grows.

DELETE FROM atlas_driver_offer_bpp.restricted_extra_fare;

INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare
       (id, merchant_id, vehicle_variant, min_trip_distance, driver_max_extra_fare)
SELECT gen_random_uuid()::text, m.id, v.variant, b.from_m, b.cap
  FROM atlas_driver_offer_bpp.merchant m
 CROSS JOIN (VALUES ('HATCHBACK'), ('SEDAN'), ('SUV'), ('AUTO_RICKSHAW'))
         AS v(variant)
 CROSS JOIN (VALUES
        --  from      cap     50% of the Economy fare at that distance
        (     0,       75),   --      150
        (  2000,      110),   --      220
        (  4000,      145),   --      290
        (  6000,      180),   --      360
        (  8000,      215),   --      430
        ( 10000,      250),   --      500
        ( 12000,      285),   --      570
        ( 15000,      335),   --      675
        ( 20000,      425),   --      850
        ( 30000,      600)    --    1 200
      ) AS b(from_m, cap);

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
