-- Migrate JuspayCfg.loyaltyProgramMap from the legacy string shape
--   { "<programId>": "LOYALTY_WALLET" }
-- to the structured shape carrying a default conversion rate
--   { "<programId>": { "programType": "LOYALTY_WALLET", "conversionRate": 1 } }
--
-- Rationale: the loyalty ledger now derives the ₹-per-point value from the
-- programs API burn rate, falling back to this per-program config default (no
-- hardcoded 1:1 in code). Existing rows store bare-string values that the new
-- LoyaltyProgramEntry parser can no longer read, so rewrite them here.
--
-- Idempotent: only string-valued entries are rewritten; already-structured
-- entries are left untouched. The default conversionRate is 1 (₹1/point) —
-- operators should update it per program to the correct rate.

DO $$
DECLARE
  r RECORD;
BEGIN
  FOR r IN
    SELECT schemaname
    FROM pg_tables
    WHERE tablename = 'merchant_service_config'
      AND schemaname IN ('atlas_app', 'atlas_driver_offer_bpp')
  LOOP
    EXECUTE format($f$
      UPDATE %I.merchant_service_config m
      SET config_json = jsonb_set(
            m.config_json::jsonb,
            '{loyaltyProgramMap}',
            (
              SELECT jsonb_object_agg(
                       e.key,
                       CASE
                         WHEN jsonb_typeof(e.value) = 'string'
                           THEN jsonb_build_object('programType', e.value, 'conversionRate', 1)
                         ELSE e.value
                       END)
              FROM jsonb_each((m.config_json::jsonb) -> 'loyaltyProgramMap') AS e
            )
          )::json,
          updated_at = now()
      WHERE (m.config_json::jsonb) ? 'loyaltyProgramMap'
        AND jsonb_typeof((m.config_json::jsonb) -> 'loyaltyProgramMap') = 'object'
        AND (m.config_json::jsonb) -> 'loyaltyProgramMap' <> '{}'::jsonb
        AND EXISTS (
          SELECT 1
          FROM jsonb_each((m.config_json::jsonb) -> 'loyaltyProgramMap') AS e
          WHERE jsonb_typeof(e.value) = 'string'
        )
    $f$, r.schemaname);
  END LOOP;
END $$;
