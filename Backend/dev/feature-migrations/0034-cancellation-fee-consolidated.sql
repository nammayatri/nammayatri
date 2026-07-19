-- ============================================================================
-- Cancellation fee — consolidated Helsinki config (supersedes 0005 / 0011 / 0031)
-- ============================================================================
-- Single source of truth for the cancellation-penalty feature. Replaces the
-- overlapping/patched setup previously spread across:
--   0005-enable-cancellation-fee-helsinki.sql
--   0011-helsinki-cancellation-and-invoice-extras.sql
--   0031-rider-cancellation-3min-rule.sql
--
-- Design (see dev/docs/cancellation-fee-configuration-guide.md):
--   * transporter_config.can_add_cancellation_fee is the single master switch.
--   * NammaTags gate ELIGIBILITY (the time/wait thresholds live ONLY here):
--       - CustomerNoShowCancellation: driver cancels with a configured penalty
--         reason and has waited >= 3 min at pickup.
--       - CustomerCancellation: rider cancels >= 3 min after booking.
--   * USER-CANCELLATION-DUES dynamic logic sets AMOUNTS ONLY (no redundant
--     threshold/reason re-checks); the driver-vs-rider amount is picked by
--     cancelledBy.
--   * New typed config fields:
--       - transporter_config.valid_cancellation_penalty_reasons  (configurable reason)
--       - transporter_config.cancellation_fee_payment_method_exceptions  (skip fee for e.g. Cash)
--       - rider_config.immediate_capture_driver_cancellation_fee / _rider_...  (capture now vs pending due)
--   * The penalty reason ('CUSTOMER_NO_SHOW') appears in three coupled places
--     seeded together below: the config field, the no-show NammaTag rule_engine,
--     and (implicitly, via cancelledBy) the dynamic logic. Change it in ONE place here.
--
-- DO NOT RUN IN MASTER/PROD. Idempotent (ON CONFLICT / dynamic city lookup).
-- Deploy BPP+BAP code BEFORE running this SQL.
-- ============================================================================

DO $$
DECLARE
  v_bpp_merchant_id TEXT;
  v_bpp_city_id     TEXT;
  v_bap_merchant_id TEXT;
  v_bap_city_id     TEXT;
BEGIN
  SELECT moc.merchant_id, moc.id INTO v_bpp_merchant_id, v_bpp_city_id
  FROM atlas_driver_offer_bpp.merchant_operating_city moc
  WHERE moc.merchant_short_id = 'BRIDGE_FINLAND_PARTNER' AND moc.city = 'Helsinki'
  LIMIT 1;

  SELECT moc.merchant_id, moc.id INTO v_bap_merchant_id, v_bap_city_id
  FROM atlas_app.merchant_operating_city moc
  WHERE moc.merchant_short_id = 'BRIDGE_FINLAND' AND moc.city = 'Helsinki'
  LIMIT 1;

  ------------------------------------------------------------
  -- BPP setup
  ------------------------------------------------------------
  IF v_bpp_merchant_id IS NULL OR v_bpp_city_id IS NULL THEN
    RAISE NOTICE 'BRIDGE_FINLAND_PARTNER Helsinki not found in BPP, skipping BPP cancellation-fee setup';
  ELSE
    -- 1. transporter_config: master switch + tax/invoice/wallet + new typed config fields
    UPDATE atlas_driver_offer_bpp.transporter_config
    SET can_add_cancellation_fee = true,
        -- Finland 24% VAT
        tax_config = '{"rideGst":{"cgstPercentage":null,"sgstPercentage":null,"igstPercentage":0.24},"defaultTdsRate":null,"serviceVatPercentage":null,"securityDepositGst":null,"subscriptionTdsRate":null}',
        -- Driver invoice: separate fee + GST lines, ledger entries on, Lynx logo
        invoice_config = '{"driverInvoiceLineItemsVatInclusive":false,"emitLedgerEntries":true,"logoUrl":"https://raw.githubusercontent.com/witcher-shailesh/github-asset-store/main/uploads/img-lynx-app-2-1778256229295.svg"}',
        -- Driver wallet for BPP-side finance ledger entries
        driver_wallet_config = '{"enableDriverWallet":true,"enableWalletPayout":true,"enableWalletTopup":false,"driverWalletPayoutThreshold":0,"forceOnlineLedger":true,"gstPercentage":0,"minimumWalletPayoutAmount":0,"payoutCutOffDays":0,"maxWalletPayoutsPerDay":null,"minWalletAmountForCashRides":null,"payoutFee":null}',
        -- NEW: configurable penalty reason(s) for driver no-show (keep in sync with the NammaTag below)
        valid_cancellation_penalty_reasons = '{CUSTOMER_NO_SHOW}'
        -- NEW: payment instruments exempt from the cancellation fee (no fee computed, none sent to BAP).
        -- Left unset here so cash rides are still charged (Helsinki charges cash cancellations).
        -- To exempt e.g. cash, uncomment:
        --   , cancellation_fee_payment_method_exceptions = '{Cash}'
    WHERE merchant_operating_city_id = v_bpp_city_id;

    -- 2. cancellation_reason row for the configured penalty reason (handler must accept it)
    INSERT INTO atlas_driver_offer_bpp.cancellation_reason
      (reason_code, description, enabled, priority, created_at, updated_at)
    VALUES
      ('CUSTOMER_NO_SHOW', 'Customer did not show up', true, 8, now(), now())
    ON CONFLICT (reason_code) DO UPDATE
      SET enabled = true, priority = EXCLUDED.priority,
          description = EXCLUDED.description, updated_at = now();

    -- 3. USER-CANCELLATION-DUES rollout (100%)
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_rollout
      (domain, percentage_rollout, version, version_description, time_bounds, merchant_operating_city_id, created_at, updated_at)
    VALUES
      ('USER-CANCELLATION-DUES', 100, 1, 'Cancellation dues for Helsinki (consolidated)', 'Unbounded', v_bpp_city_id, now(), now())
    ON CONFLICT (domain, merchant_operating_city_id, time_bounds, version) DO UPDATE SET
      percentage_rollout = EXCLUDED.percentage_rollout,
      version_description = EXCLUDED.version_description,
      updated_at = now();

    -- 4. USER-CANCELLATION-DUES logic — AMOUNTS ONLY (thresholds are enforced by NammaTags).
    --    Rider cancellation is the default amount; driver no-show overrides by cancelledBy.

    -- Order 0: base cancellation charge (rider cancellation) = 3 EUR
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element
      (description, domain, logic, "order", merchant_id, version)
    VALUES
      ('Base cancellation charge (3 EUR)', 'USER-CANCELLATION-DUES',
       '{"cat":[{"var":""},{"cancellationCharges":3}]}',
       0, v_bpp_merchant_id, 1)
    ON CONFLICT (domain, "order", version) DO UPDATE SET
      description = EXCLUDED.description, logic = EXCLUDED.logic, merchant_id = EXCLUDED.merchant_id;

    -- Order 1: base cancellation tax (24% VAT of 3 EUR) = 0.72 EUR
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element
      (description, domain, logic, "order", merchant_id, version)
    VALUES
      ('Base cancellation tax (0.72 EUR)', 'USER-CANCELLATION-DUES',
       '{"cat":[{"var":""},{"cancellationChargesTax":0.72}]}',
       1, v_bpp_merchant_id, 1)
    ON CONFLICT (domain, "order", version) DO UPDATE SET
      description = EXCLUDED.description, logic = EXCLUDED.logic, merchant_id = EXCLUDED.merchant_id;

    -- Order 2: driver no-show charge override (100 EUR when cancelledBy = driver)
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element
      (description, domain, logic, "order", merchant_id, version)
    VALUES
      ('Driver no-show charge override (100 EUR)', 'USER-CANCELLATION-DUES',
       '{"cat":[{"var":""},{"cancellationCharges":{"if":[{"==":[{"var":"cancelledBy"},"CancellationByDriver"]},100,{"var":"cancellationCharges"}]}}]}',
       2, v_bpp_merchant_id, 1)
    ON CONFLICT (domain, "order", version) DO UPDATE SET
      description = EXCLUDED.description, logic = EXCLUDED.logic, merchant_id = EXCLUDED.merchant_id;

    -- Order 3: driver no-show tax override (24 EUR when cancelledBy = driver)
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element
      (description, domain, logic, "order", merchant_id, version)
    VALUES
      ('Driver no-show tax override (24 EUR)', 'USER-CANCELLATION-DUES',
       '{"cat":[{"var":""},{"cancellationChargesTax":{"if":[{"==":[{"var":"cancelledBy"},"CancellationByDriver"]},24,{"var":"cancellationChargesTax"}]}}]}',
       3, v_bpp_merchant_id, 1)
    ON CONFLICT (domain, "order", version) DO UPDATE SET
      description = EXCLUDED.description, logic = EXCLUDED.logic, merchant_id = EXCLUDED.merchant_id;

    -- 5. Remove any stale higher-order elements from the previous 6-order setup (0005).
    DELETE FROM atlas_driver_offer_bpp.app_dynamic_logic_element
    WHERE domain = 'USER-CANCELLATION-DUES' AND version = 1 AND "order" >= 4
      AND merchant_id = v_bpp_merchant_id;

    -- 6. NammaTag: CustomerNoShowCancellation — driver cancels with penalty reason, waited >= threshold.
    --    Threshold is the driver-wait-at-pickup window. LOCAL/TEST value is 5s (integration test 08
    --    waits ~10s); PRODUCTION should use 180 (3 min). Change the `180`/`5` literal below per env.
    INSERT INTO atlas_driver_offer_bpp.namma_tag_v2
      (category, description, tag_type, merchant_operating_city_id, name, tags, rule_engine, created_at, updated_at)
    VALUES
      ('CustomerNoShowCancellationValidity',
       'Driver no-show: charge if driver cancelled with CUSTOMER_NO_SHOW after waiting >= 5s (local; prod=3min)',
       'ApplicationTag', v_bpp_city_id, 'CustomerNoShowCancellation', '{Valid,Invalid}',
       '{"if":[{"and":[{"==":[{"var":"cancellationReason.reasonCode"},"CUSTOMER_NO_SHOW"]},{"==":[{"var":"cancellationReason.source"},"ByDriver"]}]},{"if":[{">=":[{"-":[{"var":"currentTime"},{"if":[{"==":[{"var":"driverArrivalTime"},null]},{"var":"currentTime"},{"var":"driverArrivalTime"}]}]},5]},"Valid","Invalid"]},null]}',
       now(), now())
    ON CONFLICT (merchant_operating_city_id, name) DO UPDATE SET
      rule_engine = EXCLUDED.rule_engine, description = EXCLUDED.description,
      category = EXCLUDED.category, updated_at = now();

    INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger_v2 (event, merchant_operating_city_id, tag_name, created_at, updated_at)
    VALUES ('RideCancel', v_bpp_city_id, 'CustomerNoShowCancellation', now(), now())
    ON CONFLICT (event, merchant_operating_city_id, tag_name) DO UPDATE SET updated_at = now();

    -- 7. NammaTag: CustomerCancellation — rider cancels >= 3 min after booking
    INSERT INTO atlas_driver_offer_bpp.namma_tag_v2
      (category, description, tag_type, merchant_operating_city_id, name, tags, rule_engine, created_at, updated_at)
    VALUES
      ('CustomerCancellationValidity',
       'Rider cancellation: charge if rider cancels >= 3 min after booking',
       'ApplicationTag', v_bpp_city_id, 'CustomerCancellation', '{Valid,Invalid}',
       '{"if":[{"==":[{"var":"cancellationReason.source"},"ByUser"]},{"if":[{">=":[{"-":[{"var":"currentTime"},{"var":"bookingCreatedTime"}]},180]},"Valid","Invalid"]},null]}',
       now(), now())
    ON CONFLICT (merchant_operating_city_id, name) DO UPDATE SET
      rule_engine = EXCLUDED.rule_engine, description = EXCLUDED.description,
      category = EXCLUDED.category, updated_at = now();

    INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger_v2 (event, merchant_operating_city_id, tag_name, created_at, updated_at)
    VALUES ('RideCancel', v_bpp_city_id, 'CustomerCancellation', now(), now())
    ON CONFLICT (event, merchant_operating_city_id, tag_name) DO UPDATE SET updated_at = now();

    RAISE NOTICE 'BPP Helsinki cancellation fee configured (merchant=%, city=%)', v_bpp_merchant_id, v_bpp_city_id;
  END IF;

  ------------------------------------------------------------
  -- BAP setup
  ------------------------------------------------------------
  IF v_bap_merchant_id IS NULL OR v_bap_city_id IS NULL THEN
    RAISE NOTICE 'BRIDGE_FINLAND Helsinki not found in BAP, skipping BAP rider_config setup';
  ELSE
    UPDATE atlas_app.rider_config
    SET settle_cancellation_fee_before_next_ride = true,
        -- NEW: separate immediate-capture toggles for driver- vs rider-initiated fees
        immediate_capture_driver_cancellation_fee = true,
        immediate_capture_rider_cancellation_fee = true,
        -- Customer invoice branding (supplier + logo)
        invoice_config = '{"ledgerEmitEnabled":true,"logoUrl":"https://raw.githubusercontent.com/witcher-shailesh/github-asset-store/main/uploads/img-lynx-app-2-1778256229295.svg","supplierName":"LynxTaxi Finland","supplierAddress":"123, MB Road, Kochi, Helsinki - 682001, Finland","supplierVatNumber":"FI12345678"}'
    WHERE merchant_operating_city_id = v_bap_city_id;

    RAISE NOTICE 'BAP Helsinki rider_config configured (merchant=%, city=%)', v_bap_merchant_id, v_bap_city_id;
  END IF;
END $$;

-- ---------------------------------------------------------------------------
-- access_matrix grant: FLEET role -> FINANCE_INVOICE_PDF (invoice download).
-- Preserved from 0011 (non-cancellation, dashboard access). Separate DO block:
-- atlas_bpp_dashboard.role uses a different schema/key.
-- ---------------------------------------------------------------------------
INSERT INTO atlas_bpp_dashboard.access_matrix
  (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at)
SELECT
  gen_random_uuid()::text, r.id, 'DSL', 'USER_FULL_ACCESS',
  'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_PDF',
  now(), now()
FROM atlas_bpp_dashboard.role r
WHERE r.dashboard_access_type IN ('FLEET_OWNER', 'FLEET_BUSINESS')
  AND NOT EXISTS (
    SELECT 1 FROM atlas_bpp_dashboard.access_matrix am
    WHERE am.role_id = r.id
      AND am.user_action_type = 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_PDF'
  );

SELECT 'Cancellation fee consolidated config applied' AS status;
