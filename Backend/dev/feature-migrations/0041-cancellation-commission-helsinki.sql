-- ============================================================================
-- Cancellation COMMISSION — Helsinki dev seed (composes with 0034)
-- ============================================================================
-- Appends the two commission outputs to the USER-CANCELLATION-DUES ruleset that
-- 0034-cancellation-fee-consolidated.sql seeds (orders 0-3; it also DELETEs any
-- order >= 4, so this file must run AFTER it — the numbering guarantees that).
--
--   * Order 4: cancellationCommission — GROSS (VAT-inclusive), 15% of
--     (cancellationCharges + cancellationChargesTax). Var-based, so it composes
--     with whatever orders 0-3 set (incl. the driver no-show override).
--   * Order 5: overdueCancellationCommission — same 15% on the overdue charge,
--     null-guarded (null when the ruleset sets no overdue fee).
--
-- The base/VAT split happens in Haskell via tax_config.commissionVatPercentage,
-- never here. The 15% must match fare_policy.cancellation_commission_charge_config
-- (route (c) computes from the fare policy, not this JL) — keep the two in sync.
--
-- DO NOT RUN IN MASTER/PROD. Idempotent (ON CONFLICT upserts).
-- ============================================================================

DO $$
DECLARE
  v_bpp_merchant_id TEXT;
BEGIN
  SELECT moc.merchant_id INTO v_bpp_merchant_id
  FROM atlas_driver_offer_bpp.merchant_operating_city moc
  WHERE moc.merchant_short_id = 'BRIDGE_FINLAND_PARTNER' AND moc.city = 'Helsinki'
  LIMIT 1;

  -- Commission on cancellation fees is controlled entirely by the JsonLogic orders below
  -- (Maybe commission) plus the fare_policy cancellation_commission_charge_config — no
  -- separate enable flag. Absent config / JL orders => no commission (feature ships dark).

  -- Order 4: cancellation commission (gross, 15% of fee incl. VAT)
  INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element
    (description, domain, logic, "order", merchant_id, version)
  VALUES
    ('Cancellation commission (15% of fee incl. VAT, gross)', 'USER-CANCELLATION-DUES',
     '{"cat":[{"var":""},{"cancellationCommission":{"*":[0.15,{"+":[{"var":"cancellationCharges"},{"var":"cancellationChargesTax"}]}]}}]}',
     4, v_bpp_merchant_id, 1)
  ON CONFLICT (domain, "order", version) DO UPDATE SET
    description = EXCLUDED.description, logic = EXCLUDED.logic, merchant_id = EXCLUDED.merchant_id;

  -- Order 5: overdue cancellation commission (null-guarded; null when no overdue fee)
  INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element
    (description, domain, logic, "order", merchant_id, version)
  VALUES
    ('Overdue cancellation commission (15% of overdue fee incl. VAT, gross)', 'USER-CANCELLATION-DUES',
     '{"cat":[{"var":""},{"overdueCancellationCommission":{"if":[{"!=":[{"var":"overdueCancellationCharge"},null]},{"*":[0.15,{"+":[{"var":"overdueCancellationCharge"},{"var":"overdueCancellationTax"}]}]},null]}}]}',
     5, v_bpp_merchant_id, 1)
  ON CONFLICT (domain, "order", version) DO UPDATE SET
    description = EXCLUDED.description, logic = EXCLUDED.logic, merchant_id = EXCLUDED.merchant_id;
END $$;
