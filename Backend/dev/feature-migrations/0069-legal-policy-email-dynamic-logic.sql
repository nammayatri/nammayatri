-- LEGAL-POLICY-UPDATE-EMAIL dynamic logic seed.
-- Seeds a default rule for every merchant and rolls it out at 100% to every operating city
-- on BOTH BAP (atlas_app) and BPP (atlas_driver_offer_bpp) schemas. Idempotent.
-- Override per-city via dashboard LogicBuilder when custom wording is needed.
--
-- Rule contract — the AppDynamicLogic engine evaluates the merged rule against the
-- handler's `LegalPolicyEmailLogicInput` as source data. Final value must shape as:
--   { subject :: [Text], body :: [Text], fromEmail :: Maybe Text }
-- which `LegalPolicyEmailLogicOutput` deserializes. Handler concatenates subject/body
-- arrays before sending — the json-logic-hs package pinned by this repo has no
-- string-concat operator (its `cat` is a deep-merge, not a String++), so Text
-- composition happens on the Haskell receive side.
--
-- Composition shape:
--   - Top-level is a multi-key object. json-logic-hs recursively evaluates each key's value.
--   - `subject` / `body` are JSON arrays whose elements each evaluate to a String
--     (via `var`, `if`, or literal). The handler concatenates on receive.
--   - Every element inside an array must return a String — do NOT return a nested
--     Array from an `if` branch inside subject/body, or `A.fromJSON :: Result [Text]`
--     will fail with `logic_result_parse_failed`.

DO $$
DECLARE
  m RECORD;
  v_bap_logic TEXT := $json$
    {
      "subject": [
        {"if": [
          {"==": [{"var": "policyType"}, "TERMS_OF_SERVICE"]}, "Our Terms of Service have been updated",
          {"==": [{"var": "policyType"}, "PRIVACY_POLICY"]}, "Our Privacy Policy has been updated",
          {"==": [{"var": "policyType"}, "COOKIE_POLICY"]}, "Our Cookie Policy has been updated",
          {"==": [{"var": "policyType"}, "CONSENT_FORM"]}, "Updated consent terms",
          "Policy update"
        ]},
        " (v",
        {"var": "version"},
        ")"
      ],
      "body": [
        "<!DOCTYPE html><html><body style=\"font-family:Arial,sans-serif;max-width:600px;margin:auto;color:#111;\">",
        "<h2 style=\"color:#2563eb;\">Policy update</h2><p>Hi,</p>",
        {"if": [
          {"==": [{"var": "policyType"}, "TERMS_OF_SERVICE"]}, "<p>We have updated our <strong>Terms of Service</strong>.</p>",
          {"==": [{"var": "policyType"}, "PRIVACY_POLICY"]}, "<p>We have updated our <strong>Privacy Policy</strong>.</p>",
          {"==": [{"var": "policyType"}, "COOKIE_POLICY"]}, "<p>We have updated our <strong>Cookie Policy</strong>.</p>",
          {"==": [{"var": "policyType"}, "CONSENT_FORM"]}, "<p>We have updated the <strong>consent form</strong> you previously agreed to.</p>",
          "<p>We have updated our policy.</p>"
        ]},
        "<p>Version: <strong>",
        {"var": "version"},
        "</strong></p>",
        {"if": [
          {"var": "isMandatory"},
          "<p style=\"background:#fef3c7;padding:10px;border-left:4px solid #f59e0b;\">This update is mandatory. Continuing to use our services implies acceptance.</p>",
          "<p>Please review when convenient.</p>"
        ]},
        "<p><a href=\"",
        {"var": "url"},
        "\" style=\"display:inline-block;padding:10px 20px;background:#2563eb;color:#fff;text-decoration:none;border-radius:4px;\">View the full document</a></p>",
        "<p style=\"color:#666;font-size:13px;\">Or copy this link: <a href=\"",
        {"var": "url"},
        "\">",
        {"var": "url"},
        "</a></p>",
        "<hr style=\"border:none;border-top:1px solid #e5e7eb;margin:24px 0;\"/><p style=\"color:#666;font-size:12px;\">Namma Yatri</p></body></html>"
      ],
      "fromEmail": "noreply@moving.tech"
    }
  $json$;
  v_bpp_logic TEXT := $json$
    {
      "subject": [
        {"if": [
          {"and": [{"==": [{"var": "entityType"}, "DriverLegal"]}, {"==": [{"var": "policyType"}, "DRIVER_AGREEMENT"]}]}, "Driver Agreement updated",
          {"==": [{"var": "entityType"}, "FleetOwnerLegal"]}, "Fleet partner notice",
          {"==": [{"var": "policyType"}, "TERMS_OF_SERVICE"]}, "Terms of Service updated",
          {"==": [{"var": "policyType"}, "PRIVACY_POLICY"]}, "Privacy Policy updated",
          "Policy update"
        ]},
        " (v",
        {"var": "version"},
        ")"
      ],
      "body": [
        "<!DOCTYPE html><html><body style=\"font-family:Arial,sans-serif;max-width:600px;margin:auto;color:#111;\">",
        {"if": [
          {"==": [{"var": "entityType"}, "FleetOwnerLegal"]},
          "<h2 style=\"color:#059669;\">Fleet partner update</h2><p>Hello,</p>",
          "<h2 style=\"color:#2563eb;\">Partner update</h2><p>Hello Driver,</p>"
        ]},
        {"if": [
          {"==": [{"var": "policyType"}, "DRIVER_AGREEMENT"]}, "<p>Your <strong>Driver Agreement</strong> has been updated.</p>",
          {"==": [{"var": "policyType"}, "TERMS_OF_SERVICE"]}, "<p>Our <strong>Terms of Service</strong> have been updated.</p>",
          {"==": [{"var": "policyType"}, "PRIVACY_POLICY"]}, "<p>Our <strong>Privacy Policy</strong> has been updated.</p>",
          {"==": [{"var": "policyType"}, "CONSENT_FORM"]}, "<p>Please review the updated <strong>consent form</strong>.</p>",
          "<p>We have updated our policy.</p>"
        ]},
        "<p>Version: <strong>",
        {"var": "version"},
        "</strong></p>",
        {"if": [
          {"var": "isMandatory"},
          "<p style=\"background:#fef3c7;padding:10px;border-left:4px solid #f59e0b;\">This update is mandatory. You must review and accept it to keep taking rides.</p>",
          "<p>Please review when convenient.</p>"
        ]},
        "<p><a href=\"",
        {"var": "url"},
        "\" style=\"display:inline-block;padding:10px 20px;background:#059669;color:#fff;text-decoration:none;border-radius:4px;\">View the full document</a></p>",
        "<p style=\"color:#666;font-size:13px;\">Or copy this link: <a href=\"",
        {"var": "url"},
        "\">",
        {"var": "url"},
        "</a></p>",
        "<hr style=\"border:none;border-top:1px solid #e5e7eb;margin:24px 0;\"/><p style=\"color:#666;font-size:12px;\">Namma Yatri</p></body></html>"
      ],
      "fromEmail": {"if": [
        {"==": [{"var": "entityType"}, "FleetOwnerLegal"]}, "fleet-notice@moving.tech",
        "driver-notice@moving.tech"
      ]}
    }
  $json$;
BEGIN
  -- BAP (rider-app) ------------------------------------------------------------
  FOR m IN SELECT id FROM atlas_app.merchant LOOP
    INSERT INTO atlas_app.app_dynamic_logic_element
      (domain, merchant_id, version, logic, description, created_at, updated_at, "order")
    VALUES
      ('LEGAL-POLICY-UPDATE-EMAIL', m.id, 1, v_bap_logic::jsonb,
       'Default legal policy update email rule', now(), now(), 0)
    ON CONFLICT DO NOTHING;
  END LOOP;

  INSERT INTO atlas_app.app_dynamic_logic_rollout
    (domain, merchant_operating_city_id, percentage_rollout, time_bounds, version, version_description, merchant_id, created_at, updated_at)
  SELECT 'LEGAL-POLICY-UPDATE-EMAIL', moc.id, 100, 'Unbounded', 1, 'Default rule', moc.merchant_id, now(), now()
  FROM atlas_app.merchant_operating_city moc
  ON CONFLICT DO NOTHING;

  -- BPP (dynamic-offer-driver-app) ---------------------------------------------
  FOR m IN SELECT id FROM atlas_driver_offer_bpp.merchant LOOP
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element
      (domain, merchant_id, version, logic, description, created_at, updated_at, "order")
    VALUES
      ('LEGAL-POLICY-UPDATE-EMAIL', m.id, 1, v_bpp_logic::jsonb,
       'Default legal policy update email rule', now(), now(), 0)
    ON CONFLICT DO NOTHING;
  END LOOP;

  INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_rollout
    (domain, merchant_operating_city_id, percentage_rollout, time_bounds, version, version_description, merchant_id, created_at, updated_at)
  SELECT 'LEGAL-POLICY-UPDATE-EMAIL', moc.id, 100, 'Unbounded', 1, 'Default rule', moc.merchant_id, now(), now()
  FROM atlas_driver_offer_bpp.merchant_operating_city moc
  ON CONFLICT DO NOTHING;

  RAISE NOTICE 'LEGAL-POLICY-UPDATE-EMAIL seeded for all merchants on both BAP and BPP';
END $$;
