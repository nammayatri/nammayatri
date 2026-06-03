-- ============================================================================
-- earnings-test-city2-v4.sql
-- v4: outermost operator changed from "cat" to "merge".
--   - "cat" deepMerges objects/arrays => collapsed multi-branch output to a
--     single Object (or dropped branches). The consumer needs an Array.
--   - "merge" always flattens its args into a single Array, which is exactly
--     what FareUIComponentRaw[] expects.
--
-- Inner "cat"s remain (they build each single footnote Object from
-- {title}/{price}/{priority}/{section}/{isApplicable}/{uiTranslation} parts).
--
-- City:     ce6a69ac-d7c0-4393-939e-03cdfdebae5d
-- Merchant: a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e
-- ============================================================================


-- 1. TRANSLATIONS (idempotent)
INSERT INTO atlas_driver_offer_bpp.translations
  (id, message_key, language, message, created_at, updated_at)
VALUES
  ('9292a542-8e94-494f-959c-4cdc3d9246a8', 'TOLL_CHARGES_INCLUDED', 'ENGLISH', 'Toll charges of {{amount}} included',                  NOW(), NOW()),
  ('1369db33-707a-4fa3-be06-0d1c92502363', 'DISCOUNT_INCLUDED',     'ENGLISH', 'Discount of {{amount}} already included in base fare', NOW(), NOW()),
  ('23a9e58c-a896-4492-a604-6d9172b55cdd', 'COMMISSION_DEDUCTED',   'ENGLISH', 'Commission of {{amount}} deducted',                    NOW(), NOW())
ON CONFLICT (id) DO NOTHING;


-- 2. REPLACE THE RULE BODY  (delete prior, insert v4)
DELETE FROM atlas_driver_offer_bpp.app_dynamic_logic_element
WHERE domain = 'RIDE-FOOTNOTES-DISPLAY' AND "order" = 0 AND version = 1;

INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element
  (domain, "order", version, logic, description, merchant_id, patched_element, created_at, updated_at)
VALUES (
  'RIDE-FOOTNOTES-DISPLAY',
  0,
  1,
  '{"merge":[{"if":[{">":[{"if":[{"var":"bookingFareParams.tollCharges"},{"var":"bookingFareParams.tollCharges"},0]},0]},[{"cat":[{"title":"TOLL_CHARGES_INCLUDED"},{"price":{"cat":[{"amount":{"if":[{"var":"bookingFareParams.tollCharges"},{"var":"bookingFareParams.tollCharges"},0]}},{"currency":{"var":"ride.currency"}}]}},{"priority":40},{"section":"Footnote"},{"isApplicable":true},{"uiTranslation":{"cat":[{"key":"TOLL_CHARGES_INCLUDED"},{"vars":{"cat":[{"amount":{"cat":[{"if":[{"var":"bookingFareParams.tollCharges"},{"var":"bookingFareParams.tollCharges"},0]}]}}]}}]}}]}],[]]},{"if":[{">":[{"if":[{"var":"ride.discountAmount"},{"var":"ride.discountAmount"},0]},0]},[{"cat":[{"title":"DISCOUNT_INCLUDED"},{"price":{"cat":[{"amount":{"if":[{"var":"ride.discountAmount"},{"var":"ride.discountAmount"},0]}},{"currency":{"var":"ride.currency"}}]}},{"priority":41},{"section":"Footnote"},{"isApplicable":true},{"uiTranslation":{"cat":[{"key":"DISCOUNT_INCLUDED"},{"vars":{"cat":[{"amount":{"cat":[{"if":[{"var":"ride.discountAmount"},{"var":"ride.discountAmount"},0]}]}}]}}]}}]}],[]]},{"if":[{">":[{"if":[{"var":"ride.commission"},{"var":"ride.commission"},0]},0]},[{"cat":[{"title":"COMMISSION_DEDUCTED"},{"price":{"cat":[{"amount":{"if":[{"var":"ride.commission"},{"var":"ride.commission"},0]}},{"currency":{"var":"ride.currency"}}]}},{"priority":42},{"section":"Footnote"},{"isApplicable":true},{"uiTranslation":{"cat":[{"key":"COMMISSION_DEDUCTED"},{"vars":{"cat":[{"amount":{"cat":[{"if":[{"var":"ride.commission"},{"var":"ride.commission"},0]}]}}]}}]}}]}],[]]}]}',
  'helsinki footnotes v4: outermost merge (was cat); 3 always-on branches',
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  NULL,
  NOW(),
  NOW()
);

-- Rollout unchanged.


-- 3. VERIFICATION SELECTS
SELECT domain, "order", version, description, updated_at,
       jsonb_pretty(logic::jsonb) AS logic_pretty
FROM atlas_driver_offer_bpp.app_dynamic_logic_element
WHERE domain = 'RIDE-FOOTNOTES-DISPLAY'
  AND merchant_id = 'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e'
ORDER BY "order", version;

SELECT domain, merchant_operating_city_id, version, percentage_rollout,
       time_bounds, experiment_status, is_base_version, updated_at
FROM atlas_driver_offer_bpp.app_dynamic_logic_rollout
WHERE domain = 'RIDE-FOOTNOTES-DISPLAY'
  AND merchant_operating_city_id = 'ce6a69ac-d7c0-4393-939e-03cdfdebae5d';
