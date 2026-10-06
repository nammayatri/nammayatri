-- Two countries on one stack — Algeria beside Mauritania, 2026-09-13.
--
-- The pilot moved Algeria -> Mauritania on 2026-09-03 by REPLACING one with the
-- other. The client then chose to run both. This file is step 3 of that plan:
-- the data half. Nothing here needs a rebuild; accepting +213 numbers does, and
-- is step 5.
--
-- ── One merchant per country — on the DRIVER side only ─────────────────────
-- Prices live on the driver side (fare_policy is per merchant and variant,
-- nothing narrower exists in this binary), so each country needs its own
-- driver merchant. The rider side prices nothing: it keeps ONE merchant
-- (YATRI) whose service area now covers both countries.
--
-- A search in Algiers therefore reaches both driver merchants through the
-- gateway. The Mauritanian one drops it as outside its area — silently, which
-- is correct — and the Algerian one answers in dinars. The reverse for
-- Nouakchott. The app sends YATRI for both countries, and the country's own
-- driver merchant id for a driver.
--
-- ── The Algerian driver merchant is a CLONE of the Mauritanian one ────────
-- Every config row keyed by merchant_id is copied — service config (maps-shim,
-- OSRM), usage config, transporter config, fares, extra-fare caps, operating
-- city — so it behaves exactly like the merchant that is proven to work, and
-- only what differs is then set: coverage here, prices in algeria-tariff.sql.
--
-- The second merchant already in the database, nearest-drivers-testing-
-- organization, was NOT reused: it is upstream test data, it is in no registry
-- row (so it has never received a search), and its config was never ours.
--
-- ── It must be in the registry or it does not exist ────────────────────────
-- The gateway finds BPPs in atlas_registry.subscriber. Both driver merchants
-- sign with the one key in the driver app's dhall (unique_key_id
-- juspay-mobility-bpp-1-key), so the new row carries the same public key under
-- its own subscriber id.
--
-- ── The pilot's Algerian drivers move with it ──────────────────────────────
-- 13 +213 test drivers from the Algerian pilot were still filed under the
-- Mauritanian merchant, inactive. They are Algerian; they move, with their
-- vehicles. That also gives Algiers a test fleet before the rebuild.
--
-- Caches: the merchant rows are cached in Redis by both apps. Apply through
-- apply-two-countries.sh, which clears them — never FLUSHALL, which would also
-- drop every auth session and the OTP lockout counters.
--
-- Idempotent: safe to re-run.

BEGIN;

-- ── Rider side: one merchant, both countries ───────────────────────────────
UPDATE atlas_app.merchant
   SET origin_restriction      = ARRAY['Mauritania', 'Algeria'],
       destination_restriction = ARRAY['Mauritania', 'Algeria'],
       updated_at              = now()
 WHERE short_id = 'YATRI';

-- ── Driver side: the Algerian merchant ─────────────────────────────────────
INSERT INTO atlas_driver_offer_bpp.merchant
       (id, name, subscriber_id, gstin, status, verified, enabled, description,
        mobile_number, mobile_country_code, from_time, to_time, api_key,
        head_count, created_at, updated_at, info, unique_key_id, short_id,
        origin_restriction, destination_restriction)
SELECT 'algeria0-0000-0000-0000-00000algeria', 'Movin Algérie', 'MOVIN.DZ.PROVIDER',
       gstin, status, verified, enabled, description,
       -- (mobile_country_code, mobile_number) is unique across merchants.
       '9888888213', mobile_country_code, from_time, to_time, NULL,
       head_count, now(), now(), info, unique_key_id, 'MOVIN_DZ_PARTNER',
       ARRAY['Algeria'], ARRAY['Algeria']
  FROM atlas_driver_offer_bpp.merchant
 WHERE id = 'favorit0-0000-0000-0000-00000favorit'
ON CONFLICT (id) DO NOTHING;

-- Each driver merchant serves its own country and nothing else. Restated for
-- both so this file describes the whole state, not only the change.
UPDATE atlas_driver_offer_bpp.merchant
   SET origin_restriction = ARRAY['Mauritania'], destination_restriction = ARRAY['Mauritania']
 WHERE id = 'favorit0-0000-0000-0000-00000favorit';
UPDATE atlas_driver_offer_bpp.merchant
   SET origin_restriction = ARRAY['Algeria'], destination_restriction = ARRAY['Algeria']
 WHERE id = 'algeria0-0000-0000-0000-00000algeria';

-- ── Clone every config row keyed by merchant_id ────────────────────────────
-- Column lists are read from the catalogue rather than typed, so a column
-- nobody listed cannot be silently dropped from the copy. `id` gets a fresh
-- uuid, `merchant_id` the new merchant, timestamps now(). Per table, only when
-- the new merchant has no rows there yet — which is what makes a re-run safe.
DO $$
DECLARE
  src  constant text := 'favorit0-0000-0000-0000-00000favorit';
  dst  constant text := 'algeria0-0000-0000-0000-00000algeria';
  t    text;
  cols text;
  sel  text;
BEGIN
  FOREACH t IN ARRAY ARRAY['fare_policy', 'restricted_extra_fare', 'merchant_service_config',
                           'merchant_service_usage_config', 'transporter_config', 'operating_city']
  LOOP
    SELECT string_agg(quote_ident(column_name), ', ' ORDER BY ordinal_position),
           string_agg(CASE column_name
                        WHEN 'merchant_id' THEN quote_literal(dst)
                        WHEN 'id'          THEN 'gen_random_uuid()::text'
                        WHEN 'created_at'  THEN 'now()'
                        WHEN 'updated_at'  THEN 'now()'
                        ELSE quote_ident(column_name)
                      END, ', ' ORDER BY ordinal_position)
      INTO cols, sel
      FROM information_schema.columns
     WHERE table_schema = 'atlas_driver_offer_bpp' AND table_name = t;

    EXECUTE format(
      'INSERT INTO atlas_driver_offer_bpp.%I (%s) SELECT %s FROM atlas_driver_offer_bpp.%I
        WHERE merchant_id = %L
          AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.%I WHERE merchant_id = %L)',
      t, cols, sel, t, src, t, dst);
  END LOOP;
END $$;

-- ── The registry row that makes the gateway send it searches ──────────────
INSERT INTO atlas_registry.subscriber
       (unique_key_id, subscriber_id, subscriber_url, type, domain, city, country,
        status, signing_public_key, encr_public_key, valid_from, valid_until, created, updated)
SELECT unique_key_id, 'MOVIN.DZ.PROVIDER',
       'http://localhost:8016/beckn/algeria0-0000-0000-0000-00000algeria',
       type, domain, city, country, status, signing_public_key, encr_public_key,
       valid_from, valid_until, now(), now()
  FROM atlas_registry.subscriber
 WHERE subscriber_id = 'JUSPAY.MOBILITY.PROVIDER.UAT.3'
   AND NOT EXISTS (SELECT 1 FROM atlas_registry.subscriber
                    WHERE subscriber_id = 'MOVIN.DZ.PROVIDER');

-- ── The pilot's Algerian drivers, and their cars, to the Algerian merchant ─
UPDATE atlas_driver_offer_bpp.vehicle v
   SET merchant_id = 'algeria0-0000-0000-0000-00000algeria', updated_at = now()
  FROM atlas_driver_offer_bpp.person p
 WHERE p.id = v.driver_id
   AND p.mobile_country_code = '+213'
   AND v.merchant_id = 'favorit0-0000-0000-0000-00000favorit';

UPDATE atlas_driver_offer_bpp.person
   SET merchant_id = 'algeria0-0000-0000-0000-00000algeria'
 WHERE mobile_country_code = '+213'
   AND merchant_id = 'favorit0-0000-0000-0000-00000favorit';

COMMIT;
