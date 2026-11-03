-- Phase 0 of the finance-kernel unification (docs/backend/design/finance-kernel-unification-plan.md §3):
-- backfill finance_account_config for driver and fleet-owner wallets from the per-city
-- transporter_config.driver_wallet_config JSON. Values only SEED the new table — the
-- kernel's AccountConfig ladder becomes the source of truth from Phase 1; until then
-- DriverWalletConfig keeps gating behaviour, so a drifted row here is harmless.
--
-- allow_negative_balance is TRUE for driver wallets on purpose: commission on cash
-- rides may drive the wallet negative (previously accidental, now declared).

INSERT INTO atlas_driver_offer_bpp.finance_account_config
  (id, merchant_id, merchant_operating_city_id, account_type, counterparty_type, sub_ledger,
   allow_negative_balance, min_retained_balance, redeemable_cutoff,
   transfer_out_enabled, instant_transfer_out_enabled, min_transfer_out_amount,
   max_transfer_out_per_day, transfer_out_fee, transfer_in_enabled, auto_transfer_out,
   created_at, updated_at)
SELECT
  md5(random()::text || clock_timestamp()::text || tc.merchant_operating_city_id || cp.counterparty_type),
  tc.merchant_id,
  tc.merchant_operating_city_id,
  'Liability',
  cp.counterparty_type,
  NULL,
  true,
  0,
  json_build_object(
    'days', COALESCE((tc.driver_wallet_config ->> 'payoutCutOffDays')::int, 7),
    'timeOfDay', NULL,
    'tzOffsetSeconds', tc.time_diff_from_utc
  ),
  -- transfer_out: main's SWEEP gate was (enableWalletPayout OR merchant prepaid stack)
  -- with no per-person re-check, so prepaid-stack cities paid even with the city flag
  -- off — the row encodes that. INSTANT stays enableWalletPayout-only (main's
  -- ensurePayoutsEnabled gate), so prepaid-only cities sweep but cannot instant-withdraw.
  ( COALESCE((tc.driver_wallet_config ->> 'enableWalletPayout')::boolean, false)
    OR COALESCE(m.prepaid_subscription_and_wallet_enabled, false)
  ),
  COALESCE((tc.driver_wallet_config ->> 'enableWalletPayout')::boolean, false)
    AND NOT COALESCE((tc.driver_wallet_config -> 'instantPayoutExcludedRoles')::jsonb ?| cp.excluded_role_names, false),
  COALESCE((tc.driver_wallet_config ->> 'minimumWalletPayoutAmount')::numeric, 0),
  (tc.driver_wallet_config ->> 'maxWalletPayoutsPerDay')::int,
  -- Mirrors computeStripePayoutFee's WHOLE-CONFIG switch: the moment either new-model
  -- field (fixedFee / percentageRate) is present, feeType/feeValue are ignored
  -- ENTIRELY — per-field COALESCE would resurrect a legacy value the runtime discards.
  CASE
    WHEN tc.driver_wallet_config ->> 'payoutFee' IS NOT NULL THEN
      CASE
        WHEN tc.driver_wallet_config -> 'payoutFee' ->> 'fixedFee' IS NOT NULL
          OR tc.driver_wallet_config -> 'payoutFee' ->> 'percentageRate' IS NOT NULL THEN
          json_build_object(
            'bearer',
            CASE
              WHEN tc.driver_wallet_config -> 'payoutFee' ->> 'feeBearer' = 'DRIVER_BEARER' THEN 'OWNER_BEARER'
              ELSE 'PLATFORM_BEARER'
            END,
            'fixedFee', (tc.driver_wallet_config -> 'payoutFee' ->> 'fixedFee')::numeric,
            'percentageRate', (tc.driver_wallet_config -> 'payoutFee' ->> 'percentageRate')::float
          )
        ELSE
          json_build_object(
            'bearer',
            CASE
              WHEN tc.driver_wallet_config -> 'payoutFee' ->> 'feeBearer' = 'DRIVER_BEARER' THEN 'OWNER_BEARER'
              ELSE 'PLATFORM_BEARER'
            END,
            'fixedFee',
            CASE WHEN tc.driver_wallet_config -> 'payoutFee' ->> 'feeType' = 'FIXED'
                 THEN (tc.driver_wallet_config -> 'payoutFee' ->> 'feeValue')::numeric END,
            'percentageRate',
            CASE WHEN tc.driver_wallet_config -> 'payoutFee' ->> 'feeType' = 'PERCENTAGE'
                 THEN (tc.driver_wallet_config -> 'payoutFee' ->> 'feeValue')::float END
          )
      END
    ELSE NULL
  END,
  ( COALESCE((tc.driver_wallet_config ->> 'enableDriverWallet')::boolean, false)
    OR COALESCE(m.prepaid_subscription_and_wallet_enabled, false)
  ),
  NULL,
  now(),
  now()
FROM atlas_driver_offer_bpp.transporter_config tc
JOIN atlas_driver_offer_bpp.merchant m ON m.id = tc.merchant_id
CROSS JOIN (
  VALUES
    ('DRIVER', ARRAY['DRIVER']),
    ('FLEET_OWNER', ARRAY['FLEET_OWNER', 'FLEET_BUSINESS'])
) AS cp (counterparty_type, excluded_role_names)
-- City wallet switch OR the merchant-level prepaid+wallet stack: both historically
-- turned the wallet on, so both get class rows — the kernel row is now the ONLY
-- payout authority (the merchant flag no longer gates payouts in code). A NULL
-- driver_wallet_config does NOT exclude a prepaid-stack city (main's OR was satisfied
-- by the merchant flag alone); the JSON extractions above are all NULL-safe.
WHERE ( COALESCE((tc.driver_wallet_config ->> 'enableDriverWallet')::boolean, false) = true
        OR COALESCE(m.prepaid_subscription_and_wallet_enabled, false) = true
      )
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_driver_offer_bpp.finance_account_config fac
    WHERE fac.merchant_operating_city_id = tc.merchant_operating_city_id
      AND fac.account_type = 'Liability'
      AND fac.counterparty_type = cp.counterparty_type
      AND fac.sub_ledger IS NULL
  );
