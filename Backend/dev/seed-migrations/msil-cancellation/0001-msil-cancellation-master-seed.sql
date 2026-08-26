-- =====================================================================
-- MSIL cancellation — master seeding
--
-- Everything MSIL needs in master for cancellation charging to work, in one
-- transaction. The DDL (prefer_ondc_cancellation_reason_id,
-- send_ondc_cancellation_codes, booking_cancellation_reason.ondc_cancellation_reason_id)
-- and the reason-code translations ship with the branch as ordinary migrations —
-- this file is the CONFIG/DATA those migrations do not carry.
--
-- Nothing is hardcoded: merchant and city ids are resolved by short_id/city, and
-- the dynamic-logic version is computed from what already exists. Safe to re-run.
--
-- Run as a single unit. It aborts rather than half-applying.
-- =====================================================================

\set ON_ERROR_STOP on
BEGIN;

DO $$
DECLARE
  v_merchant_id text;
  v_city        record;
  v_tag         record;

BEGIN
  SELECT id INTO STRICT v_merchant_id
  FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER';

  -- ── 1. Cancellation switches + GST ────────────────────────────────
  -- preferOndcCancellationReasonId and send_ondc_cancellation_codes are a PAIR:
  -- one without the other means we either parse codes we never emit, or emit a
  -- code derived from free text.
  -- carryForwardDues and consumeRideCreditOnCancellation now live on the
  -- consequence-matrix rows (0003), and the three thresholds are literals inside
  -- the fault rule there.
  UPDATE atlas_driver_offer_bpp.transporter_config tc
  SET prefer_ondc_cancellation_reason_id = true,
      updated_at = NOW()
  FROM atlas_driver_offer_bpp.merchant_operating_city moc
  WHERE moc.id = tc.merchant_operating_city_id AND moc.merchant_id = v_merchant_id;

  -- Delhi is intra-state for MSIL (GSTIN 06AAACM0829Q5Z4): CGST+SGST apply, IGST must
  -- be NULL. Production currently has all three set, which taxes every ride fare at 10%
  -- instead of 5%. Values are FRACTIONS here (0.025 = 2.5%); serviceVatPercentage in the
  -- same object is a PERCENTAGE — do not copy one field's scale onto the other.
  UPDATE atlas_driver_offer_bpp.transporter_config tc
  SET tax_config = (tc.tax_config::jsonb || '{"rideGst":{"cgstPercentage":0.025,"sgstPercentage":0.025,"igstPercentage":null}}'::jsonb)::json,
      updated_at = NOW()
  FROM atlas_driver_offer_bpp.merchant_operating_city moc
  WHERE moc.id = tc.merchant_operating_city_id AND moc.merchant_id = v_merchant_id;

  -- ── 2. Beckn config: BAP collects, and we emit ONDC codes on on_cancel ──
  UPDATE atlas_driver_offer_bpp.beckn_config
  SET send_ondc_cancellation_codes = true, collected_by = 'BAP', updated_at = NOW()
  WHERE merchant_id = v_merchant_id;

  -- ── 3. Fee matrix ─────────────────────────────────────────────────
  -- Charging now runs off CANCELLATION-FAULT-VERDICT + the consequence matrix,
  -- both seeded by 0003. USER-CANCELLATION-DUES no longer runs.

  FOR v_city IN
    SELECT id, city FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_id = v_merchant_id
  LOOP
    -- ── 4. Cancellation tags, PER CITY ──────────────────────────────
    -- namma_tag_v2 / namma_tag_trigger_v2 are scoped by merchant_operating_city_id.
    -- Without these the charge gate never opens and NOTHING is ever charged, whatever
    -- the fee matrix says. Verify each city in master: a city missing RideCancel
    -- triggers silently charges nobody.
    INSERT INTO atlas_driver_offer_bpp.namma_tag_v2
      (name, category, description, chakra, tag_type, merchant_operating_city_id, tags, rule_engine, created_at, updated_at)
    SELECT 'CustomerNoShowCancellation', 'CustomerNoShowCancellationValidity',
           'Customer failed to arrive within the acceptable wait period', NULL, 'ApplicationTag',
           v_city.id, '{Valid,Invalid}',
           '[{"if":[{"and":[{"==":[{"var":"cancellationReason.source"},"ByDriver"]},{"==":[{"var":"cancellationReason.reasonCode"},"DRIVER_CANCEL_CUSTOMER_NO_SHOW"]}]},"Valid","Invalid"]}]',
           NOW(), NOW()
    WHERE NOT EXISTS (
      SELECT 1 FROM atlas_driver_offer_bpp.namma_tag_v2
      WHERE name = 'CustomerNoShowCancellation' AND merchant_operating_city_id = v_city.id);

    INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger_v2
      (event, merchant_operating_city_id, tag_name, created_at, updated_at)
    SELECT 'RideCancel', v_city.id, 'CustomerNoShowCancellation', NOW(), NOW()
    WHERE NOT EXISTS (
      SELECT 1 FROM atlas_driver_offer_bpp.namma_tag_trigger_v2
      WHERE event = 'RideCancel' AND tag_name = 'CustomerNoShowCancellation'
        AND merchant_operating_city_id = v_city.id);

    -- Backfill the two baseline cancellation tags for any city that lacks them, copying
    -- the rule_engine from a sibling MSIL city that already has it. Hyderabad ships with
    -- ZERO RideCancel triggers, which is why it charges nobody today. Copying rather than
    -- inventing keeps every MSIL city on identical validity rules.
    FOR v_tag IN SELECT unnest(ARRAY['DriverCancellation','CustomerCancellation']) AS name
    LOOP
      IF NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.namma_tag_v2
                     WHERE name = v_tag.name AND merchant_operating_city_id = v_city.id) THEN
        INSERT INTO atlas_driver_offer_bpp.namma_tag_v2
          (name, category, description, chakra, tag_type, merchant_operating_city_id, tags, rule_engine, created_at, updated_at)
        SELECT src.name, src.category, src.description, src.chakra, src.tag_type,
               v_city.id, src.tags, src.rule_engine, NOW(), NOW()
        FROM atlas_driver_offer_bpp.namma_tag_v2 src
        JOIN atlas_driver_offer_bpp.merchant_operating_city m2 ON m2.id = src.merchant_operating_city_id
        WHERE src.name = v_tag.name AND m2.merchant_id = v_merchant_id
        LIMIT 1;

        IF NOT FOUND THEN
          RAISE WARNING 'No MSIL city has tag % to copy — % will not charge until it is seeded manually',
            v_tag.name, v_city.city;
        END IF;
      END IF;

      INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger_v2
        (event, merchant_operating_city_id, tag_name, created_at, updated_at)
      SELECT 'RideCancel', v_city.id, v_tag.name, NOW(), NOW()
      WHERE EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.namma_tag_v2
                    WHERE name = v_tag.name AND merchant_operating_city_id = v_city.id)
        AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.namma_tag_trigger_v2
                        WHERE event = 'RideCancel' AND tag_name = v_tag.name
                          AND merchant_operating_city_id = v_city.id);
    END LOOP;

    RAISE NOTICE 'MSIL % : cancellation tags seeded', v_city.city;
  END LOOP;

  RAISE NOTICE 'MSIL cancellation config + tags seeded (rules/matrix: see 0003)';
END $$;

COMMIT;

-- =====================================================================
-- VERIFY  (all four should look right before you walk away)
-- =====================================================================

-- 1. config per city
SELECT moc.city,
       tc.prefer_ondc_cancellation_reason_id,
       tc.tax_config->'rideGst' AS ride_gst
FROM atlas_driver_offer_bpp.transporter_config tc
JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = tc.merchant_operating_city_id
JOIN atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
WHERE m.short_id = 'MSIL_PARTNER';

-- 2. beckn config — expect BAP / true on every vehicle category
SELECT bc.vehicle_category, bc.collected_by, bc.send_ondc_cancellation_codes
FROM atlas_driver_offer_bpp.beckn_config bc
JOIN atlas_driver_offer_bpp.merchant m ON m.id = bc.merchant_id
WHERE m.short_id = 'MSIL_PARTNER';

-- 3. the fault rule resolves for every MSIL city (seeded by 0003)
SELECT moc.city, r.version, r.percentage_rollout, e.description
FROM atlas_driver_offer_bpp.app_dynamic_logic_rollout r
JOIN atlas_driver_offer_bpp.app_dynamic_logic_element e
  ON e.domain = r.domain AND e.version = r.version
JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = r.merchant_operating_city_id
JOIN atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
WHERE m.short_id = 'MSIL_PARTNER' AND r.domain = 'CANCELLATION-FAULT-VERDICT';

-- 4. RideCancel tags per city.
--    MUST include DriverCancellation, CustomerCancellation and CustomerNoShowCancellation.
--    A city missing any of these will silently never charge — this is the single most
--    likely reason "the fee matrix is seeded but nothing is charged".
SELECT moc.city, t.tag_name
FROM atlas_driver_offer_bpp.namma_tag_trigger_v2 t
JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = t.merchant_operating_city_id
JOIN atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
WHERE m.short_id = 'MSIL_PARTNER' AND t.event = 'RideCancel'
ORDER BY moc.city, t.tag_name;

-- =====================================================================
-- AFTER RUNNING: the driver-app caches TransporterConfig in Redis AND in-process.
-- A restart alone is NOT enough — Redis survives it. Do this order:
--   1. run this file
--   2. DEL driver-offer:CachedQueries:TransporterConfig:MerchantOperatingCityId-<moc>
--      DEL dynamic-offer-driver-app:ConfigPilot:TransporterConfig
--   3. restart driver-app  (in-process cache refills from Redis, so order matters)
--
-- ROLLBACK: set prefer_ondc_cancellation_reason_id back to NULL — the resolver falls
-- back to short_desc when it is absent. Rules and matrix rows roll back with 0003.
-- =====================================================================
