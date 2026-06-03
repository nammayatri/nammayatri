-- ============================================================================
-- earnings-test-city2-v3.sql
-- MINIMAL: 3 always-on footnotes only (toll, discount, commission).
-- Purpose: isolate whether the json-logic STRUCTURE is correct, before
-- reinstating the 19 diff branches from v2.
--
-- City:     ce6a69ac-d7c0-4393-939e-03cdfdebae5d
-- Merchant: a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e
-- ============================================================================


-- 1. TRANSLATIONS (idempotent; safe to re-run after v2 already inserted them)
INSERT INTO atlas_driver_offer_bpp.translations
  (id, message_key, language, message, created_at, updated_at)
VALUES
  ('9292a542-8e94-494f-959c-4cdc3d9246a8', 'TOLL_CHARGES_INCLUDED', 'ENGLISH', 'Toll charges of {{amount}} included',                  NOW(), NOW()),
  ('1369db33-707a-4fa3-be06-0d1c92502363', 'DISCOUNT_INCLUDED',     'ENGLISH', 'Discount of {{amount}} already included in base fare', NOW(), NOW()),
  ('23a9e58c-a896-4492-a604-6d9172b55cdd', 'COMMISSION_DEDUCTED',   'ENGLISH', 'Commission of {{amount}} deducted',                    NOW(), NOW())
ON CONFLICT (id) DO NOTHING;


-- 2. REPLACE THE RULE BODY  (delete prior, insert v3)
DELETE FROM atlas_driver_offer_bpp.app_dynamic_logic_element
WHERE domain = 'RIDE-FOOTNOTES-DISPLAY' AND "order" = 0 AND version = 1;

INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element
  (domain, "order", version, logic, description, merchant_id, patched_element, created_at, updated_at)
VALUES (
  'RIDE-FOOTNOTES-DISPLAY',
  0,
  1,
  '{"cat":[{"if":[{">":[{"if":[{"var":"bookingFareParams.tollCharges"},{"var":"bookingFareParams.tollCharges"},0]},0]},[{"cat":[{"title":"TOLL_CHARGES_INCLUDED"},{"price":{"cat":[{"amount":{"if":[{"var":"bookingFareParams.tollCharges"},{"var":"bookingFareParams.tollCharges"},0]}},{"currency":{"var":"ride.currency"}}]}},{"priority":40},{"section":"Footnote"},{"isApplicable":true},{"uiTranslation":{"cat":[{"key":"TOLL_CHARGES_INCLUDED"},{"vars":{"cat":[{"amount":{"cat":[{"if":[{"var":"bookingFareParams.tollCharges"},{"var":"bookingFareParams.tollCharges"},0]}]}}]}}]}}]}],[]]},{"if":[{">":[{"if":[{"var":"ride.discountAmount"},{"var":"ride.discountAmount"},0]},0]},[{"cat":[{"title":"DISCOUNT_INCLUDED"},{"price":{"cat":[{"amount":{"if":[{"var":"ride.discountAmount"},{"var":"ride.discountAmount"},0]}},{"currency":{"var":"ride.currency"}}]}},{"priority":41},{"section":"Footnote"},{"isApplicable":true},{"uiTranslation":{"cat":[{"key":"DISCOUNT_INCLUDED"},{"vars":{"cat":[{"amount":{"cat":[{"if":[{"var":"ride.discountAmount"},{"var":"ride.discountAmount"},0]}]}}]}}]}}]}],[]]},{"if":[{">":[{"if":[{"var":"ride.commission"},{"var":"ride.commission"},0]},0]},[{"cat":[{"title":"COMMISSION_DEDUCTED"},{"price":{"cat":[{"amount":{"if":[{"var":"ride.commission"},{"var":"ride.commission"},0]}},{"currency":{"var":"ride.currency"}}]}},{"priority":42},{"section":"Footnote"},{"isApplicable":true},{"uiTranslation":{"cat":[{"key":"COMMISSION_DEDUCTED"},{"vars":{"cat":[{"amount":{"cat":[{"if":[{"var":"ride.commission"},{"var":"ride.commission"},0]}]}}]}}]}}]}],[]]}]}',
  'helsinki footnotes v3: minimal 3 always-on (toll/discount/commission)',
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  NULL,
  NOW(),
  NOW()
);

-- Rollout unchanged (already 100% on version=1).


-- ============================================================================
-- 3. VERIFICATION SELECTS
-- ============================================================================

-- 3a. Confirm the rule body is the v3 logic
SELECT domain, "order", version, description, updated_at,
       jsonb_pretty(logic::jsonb) AS logic_pretty
FROM atlas_driver_offer_bpp.app_dynamic_logic_element
WHERE domain = 'RIDE-FOOTNOTES-DISPLAY'
  AND merchant_id = 'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e'
ORDER BY "order", version;

-- 3b. Confirm rollout is active on version=1 at 100% for this city
SELECT domain, merchant_operating_city_id, version, percentage_rollout,
       time_bounds, experiment_status, is_base_version, updated_at
FROM atlas_driver_offer_bpp.app_dynamic_logic_rollout
WHERE domain = 'RIDE-FOOTNOTES-DISPLAY'
  AND merchant_operating_city_id = 'ce6a69ac-d7c0-4393-939e-03cdfdebae5d';

-- 3c. Confirm the 3 translations exist
SELECT id, message_key, language, message
FROM atlas_driver_offer_bpp.translations
WHERE message_key IN ('TOLL_CHARGES_INCLUDED','DISCOUNT_INCLUDED','COMMISSION_DEDUCTED')
  AND language = 'ENGLISH'
ORDER BY message_key;
