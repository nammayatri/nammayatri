-- Algeria tariff, in dinars — keyed to the ALGERIAN driver merchant.
--
-- ── Two countries since 2026-09-13 ─────────────────────────────────────────
-- This file used to price every merchant in the table at once, deliberately:
-- there was one country and two merchants that both dispatched. Now each
-- country is its own merchant (two-countries-merchants.sql), and an unfiltered
-- statement here would reprice Mauritania in dinars. Every statement names
-- algeria0. The Mauritanian prices are mauritania-tariff.sql, keyed to favorit0.
--
-- ── The figures: the Mauritanian tariff converted back ─────────────────────
-- The client, 2026-09-13: "convert the existing Mauritanian prices to Algerian
-- prices". So every figure is the Mauritanian one DIVIDED by 0.30 (the rate that
-- produced it on 2026-09-03) and rounded to the nearest 5.
--
--   App name          Variant         Start   Per km   Pickup
--   Voiture           SEDAN             150       50       65
--   Scooter           AUTO_RICKSHAW     100       35       50
--   Herbin            HATCHBACK         100       35       50
--   Fourgon           SUV               200       65      100
--
-- That is close to, NOT the same as, the table the client set for Algeria on
-- 2026-08-13 (Voiture 150/45/70, Fourgon 200/60/100). The Mauritanian figures
-- were rounded on the way out, so the round trip does not land exactly home.
-- The instruction was to convert, so this converts; the old table is in git.
--
-- A herbin is priced exactly like a scooter, as the waw is in Mauritania.
--
-- `base_distance_meters` is 0: per-km runs from the first metre, the start is a
-- flat charge on top — as in both previous tariffs.
--
-- Idempotent: re-running sets the same values.

BEGIN;

-- ── Voiture ────────────────────────────────────────────────────────────────
UPDATE atlas_driver_offer_bpp.fare_policy
   SET base_distance_fare   = 150,
       base_distance_meters = 0,
       per_extra_km_fare    = 50,
       dead_km_fare         = 65,
       driver_min_extra_fee = 0,
       driver_max_extra_fee = 300,
       updated_at           = now()
 WHERE vehicle_variant = 'SEDAN'
   AND merchant_id = 'algeria0-0000-0000-0000-00000algeria';

-- ── Herbin ─────────────────────────────────────────────────────────────────
UPDATE atlas_driver_offer_bpp.fare_policy
   SET base_distance_fare   = 100,
       base_distance_meters = 0,
       per_extra_km_fare    = 35,
       dead_km_fare         = 50,
       driver_min_extra_fee = 0,
       driver_max_extra_fee = 300,
       updated_at           = now()
 WHERE vehicle_variant = 'HATCHBACK'
   AND merchant_id = 'algeria0-0000-0000-0000-00000algeria';

-- ── Fourgon ────────────────────────────────────────────────────────────────
UPDATE atlas_driver_offer_bpp.fare_policy
   SET base_distance_fare   = 200,
       base_distance_meters = 0,
       per_extra_km_fare    = 65,
       dead_km_fare         = 100,
       driver_min_extra_fee = 0,
       driver_max_extra_fee = 300,
       updated_at           = now()
 WHERE vehicle_variant = 'SUV'
   AND merchant_id = 'algeria0-0000-0000-0000-00000algeria';

-- ── Scooter ────────────────────────────────────────────────────────────────
UPDATE atlas_driver_offer_bpp.fare_policy
   SET base_distance_fare   = 100,
       base_distance_meters = 0,
       per_extra_km_fare    = 35,
       dead_km_fare         = 50,
       driver_min_extra_fee = 0,
       driver_max_extra_fee = 300,
       updated_at           = now()
 WHERE vehicle_variant = 'AUTO_RICKSHAW'
   AND merchant_id = 'algeria0-0000-0000-0000-00000algeria';

-- ── The cap the backend obeys, growing with distance ───────────────────────
-- The Mauritanian bands divided by 0.30, rounded to 5 — about half the Herbin
-- fare at each band's lower bound, which is the client's "at most half the
-- fare" rule sized against the cheapest type (the backend resolves one cap per
-- search, whatever the vehicle; measured 2026-08-13).

DELETE FROM atlas_driver_offer_bpp.restricted_extra_fare
 WHERE merchant_id = 'algeria0-0000-0000-0000-00000algeria';

INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare
       (id, merchant_id, vehicle_variant, min_trip_distance, driver_max_extra_fare)
SELECT gen_random_uuid()::text, m.id, v.variant, b.from_m, b.cap
  FROM atlas_driver_offer_bpp.merchant m
 CROSS JOIN (VALUES ('HATCHBACK'), ('SEDAN'), ('SUV'), ('AUTO_RICKSHAW'))
         AS v(variant)
 CROSS JOIN (VALUES
        --  from      cap
        (     0,       85),
        (  2000,      115),
        (  4000,      150),
        (  6000,      185),
        (  8000,      215),
        ( 10000,      250),
        ( 12000,      285),
        ( 15000,      335),
        ( 20000,      435),
        ( 30000,      600)
      ) AS b(from_m, cap)
 WHERE m.id = 'algeria0-0000-0000-0000-00000algeria';

COMMIT;
