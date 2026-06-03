-- ============================================================================
-- earnings-test-city2-v5.sql
-- v5: SQL-only fix for API still returning "footnotes": []
--
-- Problem: v4 rule emitted  vars: {"amount": <Number>}  but Haskell parses
-- vars as HashMap Text Text. Number values blow up the decode, the whole
-- [FareUIComponentRaw] fails, consumer returns [].
--
-- Fix: emit vars with STRING values only. We use `{"var":"ride.currency"}`
-- which returns the JSON String "EUR" — Aeson decodes it as Text fine.
-- Translation messages reference {{currency}} for the unit and the numeric
-- amount is carried separately in the footnote's `price` field, which the
-- UI renders alongside the text.
--
-- City:     ce6a69ac-d7c0-4393-939e-03cdfdebae5d
-- Merchant: a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e
-- ============================================================================


-- 1. TRANSLATIONS — rewrite to use {{currency}} placeholder (String).
UPDATE atlas_driver_offer_bpp.translations
SET message = 'Toll charges (in {{currency}}) are included in the fare',
    updated_at = NOW()
WHERE id = '9292a542-8e94-494f-959c-4cdc3d9246a8';

UPDATE atlas_driver_offer_bpp.translations
SET message = 'Discount (in {{currency}}) already included in the base fare',
    updated_at = NOW()
WHERE id = '1369db33-707a-4fa3-be06-0d1c92502363';

UPDATE atlas_driver_offer_bpp.translations
SET message = 'Commission (in {{currency}}) deducted from your earnings',
    updated_at = NOW()
WHERE id = '23a9e58c-a896-4492-a604-6d9172b55cdd';

-- Idempotent insert in case rows do not exist.
INSERT INTO atlas_driver_offer_bpp.translations
  (id, message_key, language, message, created_at, updated_at)
VALUES
  ('9292a542-8e94-494f-959c-4cdc3d9246a8', 'TOLL_CHARGES_INCLUDED', 'ENGLISH', 'Toll charges ({{currency}}) are included in the fare',     NOW(), NOW()),
  ('1369db33-707a-4fa3-be06-0d1c92502363', 'DISCOUNT_INCLUDED',     'ENGLISH', 'Discount ({{currency}}) already included in the base fare', NOW(), NOW()),
  ('23a9e58c-a896-4492-a604-6d9172b55cdd', 'COMMISSION_DEDUCTED',   'ENGLISH', 'Commission ({{currency}}) deducted from your earnings',     NOW(), NOW())
ON CONFLICT (id) DO NOTHING;


-- 2. REPLACE THE RULE BODY
-- vars now carries {"currency": "EUR"} via {"var":"ride.currency"} (String).
-- price still carries the dynamic numeric amount + currency separately.
DELETE FROM atlas_driver_offer_bpp.app_dynamic_logic_element
WHERE domain = 'RIDE-FOOTNOTES-DISPLAY' AND "order" = 0 AND version = 1;

INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element
  (domain, "order", version, logic, description, merchant_id, patched_element, created_at, updated_at)
VALUES (
  'RIDE-FOOTNOTES-DISPLAY',
  0,
  1,
  '{"merge":[{"if":[{">":[{"if":[{"var":"bookingFareParams.tollCharges"},{"var":"bookingFareParams.tollCharges"},0]},0]},[{"cat":[{"title":"TOLL_CHARGES_INCLUDED"},{"price":{"cat":[{"amount":{"if":[{"var":"bookingFareParams.tollCharges"},{"var":"bookingFareParams.tollCharges"},0]}},{"currency":{"var":"ride.currency"}}]}},{"priority":40},{"section":"Footnote"},{"isApplicable":true},{"uiTranslation":{"cat":[{"key":"TOLL_CHARGES_INCLUDED"},{"vars":{"cat":[{"currency":{"var":"ride.currency"}}]}}]}}]}],[]]},{"if":[{">":[{"if":[{"var":"ride.discountAmount"},{"var":"ride.discountAmount"},0]},0]},[{"cat":[{"title":"DISCOUNT_INCLUDED"},{"price":{"cat":[{"amount":{"if":[{"var":"ride.discountAmount"},{"var":"ride.discountAmount"},0]}},{"currency":{"var":"ride.currency"}}]}},{"priority":41},{"section":"Footnote"},{"isApplicable":true},{"uiTranslation":{"cat":[{"key":"DISCOUNT_INCLUDED"},{"vars":{"cat":[{"currency":{"var":"ride.currency"}}]}}]}}]}],[]]},{"if":[{">":[{"if":[{"var":"ride.commission"},{"var":"ride.commission"},0]},0]},[{"cat":[{"title":"COMMISSION_DEDUCTED"},{"price":{"cat":[{"amount":{"if":[{"var":"ride.commission"},{"var":"ride.commission"},0]}},{"currency":{"var":"ride.currency"}}]}},{"priority":42},{"section":"Footnote"},{"isApplicable":true},{"uiTranslation":{"cat":[{"key":"COMMISSION_DEDUCTED"},{"vars":{"cat":[{"currency":{"var":"ride.currency"}}]}}]}}]}],[]]}]}',
  'helsinki footnotes v5: vars uses {{currency}} (String) only',
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  NULL,
  NOW(),
  NOW()
);


-- 3. VERIFICATION
SELECT id, message_key, language, message
FROM atlas_driver_offer_bpp.translations
WHERE id IN ('9292a542-8e94-494f-959c-4cdc3d9246a8',
             '1369db33-707a-4fa3-be06-0d1c92502363',
             '23a9e58c-a896-4492-a604-6d9172b55cdd')
ORDER BY message_key;

SELECT domain, "order", version, description, updated_at,
       jsonb_pretty(logic::jsonb) AS logic_pretty
FROM atlas_driver_offer_bpp.app_dynamic_logic_element
WHERE domain = 'RIDE-FOOTNOTES-DISPLAY'
  AND merchant_id = 'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e'
ORDER BY "order", version;
