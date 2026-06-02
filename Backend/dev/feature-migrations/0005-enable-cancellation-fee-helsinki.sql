-- Enable cancellation fee feature for Helsinki (BRIDGE_FINLAND / BRIDGE_FINLAND_PARTNER)
-- Merchant / city ids are resolved dynamically from merchant_operating_city:
--   BPP: merchant_short_id = 'BRIDGE_FINLAND_PARTNER', city = 'Helsinki' (atlas_driver_offer_bpp)
--   BAP: merchant_short_id = 'BRIDGE_FINLAND',         city = 'Helsinki' (atlas_app)

DO $$
DECLARE
  v_bpp_merchant_id TEXT;
  v_bpp_city_id TEXT;
  v_bap_merchant_id TEXT;
  v_bap_city_id TEXT;
BEGIN
  -- Resolve BPP (BRIDGE_FINLAND_PARTNER / Helsinki)
  SELECT moc.merchant_id, moc.id INTO v_bpp_merchant_id, v_bpp_city_id
  FROM atlas_driver_offer_bpp.merchant_operating_city moc
  WHERE moc.merchant_short_id = 'BRIDGE_FINLAND_PARTNER' AND moc.city = 'Helsinki'
  LIMIT 1;

  -- Resolve BAP (BRIDGE_FINLAND / Helsinki)
  SELECT moc.merchant_id, moc.id INTO v_bap_merchant_id, v_bap_city_id
  FROM atlas_app.merchant_operating_city moc
  WHERE moc.merchant_short_id = 'BRIDGE_FINLAND' AND moc.city = 'Helsinki'
  LIMIT 1;

  ------------------------------------------------------------
  -- BPP setup
  ------------------------------------------------------------
  IF v_bpp_merchant_id IS NULL OR v_bpp_city_id IS NULL THEN
    RAISE NOTICE 'BRIDGE_FINLAND_PARTNER Helsinki not found in BPP merchant_operating_city, skipping BPP cancellation-fee setup';
  ELSE
    -- BPP: Enable cancellation fee + VAT config in transporter_config
    UPDATE atlas_driver_offer_bpp.transporter_config
    SET can_add_cancellation_fee = true,
        -- Finland 24% VAT (as igstPercentage since it's a single-rate international VAT)
        tax_config = '{"rideGst":{"cgstPercentage":null,"sgstPercentage":null,"igstPercentage":0.24},"defaultTdsRate":null,"serviceVatPercentage":null,"securityDepositGst":null,"subscriptionTdsRate":null}',
        -- BPP driver invoice: single "Cancellation Fee (Incl. VAT)" line item
        invoice_config = '{"driverInvoiceLineItemsVatInclusive":true,"emitLedgerEntries":true}',
        -- Enable driver wallet for BPP-side finance ledger entries
        driver_wallet_config = '{"enableDriverWallet":true,"enableWalletPayout":true,"enableWalletTopup":false,"driverWalletPayoutThreshold":0,"forceOnlineLedger":true,"gstPercentage":0,"minimumWalletPayoutAmount":0,"payoutCutOffDays":0,"maxWalletPayoutsPerDay":null,"minWalletAmountForCashRides":null,"payoutFee":null}'
    WHERE merchant_operating_city_id = v_bpp_city_id;

    -- BPP: USER-CANCELLATION-DUES dynamic logic rollout (100%)
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_rollout (
      domain, percentage_rollout, version, version_description,
      time_bounds, merchant_operating_city_id, created_at, updated_at
    ) VALUES (
      'USER-CANCELLATION-DUES', 100, 1, 'User cancellation dues for Helsinki',
      'Unbounded', v_bpp_city_id, now(), now()
    ) ON CONFLICT (domain, merchant_operating_city_id, time_bounds, version) DO UPDATE SET
      percentage_rollout = EXCLUDED.percentage_rollout,
      version_description = EXCLUDED.version_description,
      updated_at = now();

    -- BPP: Dynamic logic elements for cancellation charges

    -- Order 0: Set base cancellation charge (3 EUR, or 5 for AUTO_RICKSHAW frequent canceller)
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element (
      description, domain, logic, "order", merchant_id, version
    ) VALUES (
      'Base cancellation charge',
      'USER-CANCELLATION-DUES',
      '{"cat":[{"var":""},{"if":[{"and":[{">=":[{"var":"totalBookings"},3]},{">=":[{"var":"cancelledRides"},2]},{"==":[{"var":"serviceTier"},"AUTO_RICKSHAW"]}]},{"cancellationCharges":5},{"cancellationCharges":3}]}]}',
      0,
      v_bpp_merchant_id,
      1
    ) ON CONFLICT (domain, "order", version) DO UPDATE SET
      description = EXCLUDED.description,
      logic = EXCLUDED.logic,
      merchant_id = EXCLUDED.merchant_id;

    -- Order 1: Set cancellation tax (24% VAT: 0.72 or 1.20)
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element (
      description, domain, logic, "order", merchant_id, version
    ) VALUES (
      'Cancellation charge tax (24% VAT)',
      'USER-CANCELLATION-DUES',
      '{"cat":[{"var":""},{"if":[{"and":[{">=":[{"var":"totalBookings"},3]},{">=":[{"var":"cancelledRides"},2]},{"==":[{"var":"serviceTier"},"AUTO_RICKSHAW"]}]},{"cancellationChargesTax":1.2},{"cancellationChargesTax":0.72}]}]}',
      1,
      v_bpp_merchant_id,
      1
    ) ON CONFLICT (domain, "order", version) DO UPDATE SET
      description = EXCLUDED.description,
      logic = EXCLUDED.logic,
      merchant_id = EXCLUDED.merchant_id;

    -- Order 2: Override for CUSTOMER_NO_SHOW (driver cancels) — base=100
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element (
      description, domain, logic, "order", merchant_id, version
    ) VALUES (
      'Customer no-show charge override',
      'USER-CANCELLATION-DUES',
      '{"cat":[{"var":""},{"cancellationCharges":{"if":[{"and":[{"==":[{"var":"cancellationReasonSelected"},"CUSTOMER_NO_SHOW"]},{"==":[{"var":"cancelledBy"},"CancellationByDriver"]}]},{"if":[{"and":[{"==":[{"var":"isArrivedAtPickup"},true]},{">=":[{"var":"driverWaitingTime"},5]}]},100,0]},{"if":[{"var":"cancellationCharges"},{"var":"cancellationCharges"},0]}]}}]}',
      2,
      v_bpp_merchant_id,
      1
    ) ON CONFLICT (domain, "order", version) DO UPDATE SET
      description = EXCLUDED.description,
      logic = EXCLUDED.logic,
      merchant_id = EXCLUDED.merchant_id;

    -- Order 3: Override for CUSTOMER_NO_SHOW — tax=24
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element (
      description, domain, logic, "order", merchant_id, version
    ) VALUES (
      'Customer no-show tax override',
      'USER-CANCELLATION-DUES',
      '{"cat":[{"var":""},{"cancellationChargesTax":{"if":[{"and":[{"==":[{"var":"cancellationReasonSelected"},"CUSTOMER_NO_SHOW"]},{"==":[{"var":"cancelledBy"},"CancellationByDriver"]}]},{"if":[{"and":[{"==":[{"var":"isArrivedAtPickup"},true]},{">=":[{"var":"driverWaitingTime"},5]}]},24,0]},{"if":[{"var":"cancellationChargesTax"},{"var":"cancellationChargesTax"},0]}]}}]}',
      3,
      v_bpp_merchant_id,
      1
    ) ON CONFLICT (domain, "order", version) DO UPDATE SET
      description = EXCLUDED.description,
      logic = EXCLUDED.logic,
      merchant_id = EXCLUDED.merchant_id;

    -- Order 4: Rider cancellation — keep charges if driver arrived + waited, else 0
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element (
      description, domain, logic, "order", merchant_id, version
    ) VALUES (
      'Rider cancellation charge',
      'USER-CANCELLATION-DUES',
      '{"cat":[{"var":""},{"cancellationCharges":{"if":[{"!=":[{"var":"cancellationReasonSelected"},"CUSTOMER_NO_SHOW"]},{"if":[{"and":[{"==":[{"var":"isArrivedAtPickup"},true]},{">=":[{"var":"driverWaitingTime"},5]}]},{"if":[{"var":"cancellationCharges"},{"var":"cancellationCharges"},0]},0]},{"var":"cancellationCharges"}]}}]}',
      4,
      v_bpp_merchant_id,
      1
    ) ON CONFLICT (domain, "order", version) DO UPDATE SET
      description = EXCLUDED.description,
      logic = EXCLUDED.logic,
      merchant_id = EXCLUDED.merchant_id;

    -- Order 5: Rider cancellation — keep tax if driver arrived + waited, else 0
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element (
      description, domain, logic, "order", merchant_id, version
    ) VALUES (
      'Rider cancellation tax',
      'USER-CANCELLATION-DUES',
      '{"cat":[{"var":""},{"cancellationChargesTax":{"if":[{"!=":[{"var":"cancellationReasonSelected"},"CUSTOMER_NO_SHOW"]},{"if":[{"and":[{"==":[{"var":"isArrivedAtPickup"},true]},{">=":[{"var":"driverWaitingTime"},5]}]},{"if":[{"var":"cancellationChargesTax"},{"var":"cancellationChargesTax"},0]},0]},{"var":"cancellationChargesTax"}]}}]}',
      5,
      v_bpp_merchant_id,
      1
    ) ON CONFLICT (domain, "order", version) DO UPDATE SET
      description = EXCLUDED.description,
      logic = EXCLUDED.logic,
      merchant_id = EXCLUDED.merchant_id;

    -- BPP: Namma tag (v2) for CustomerNoShowCancellation validation
    -- App exclusively reads from namma_tag_v2 (city-specific)
    INSERT INTO atlas_driver_offer_bpp.namma_tag_v2 (
      category, description, tag_type, merchant_operating_city_id, name,
      tags, rule_engine, created_at, updated_at
    ) VALUES (
      'CustomerNoShowCancellationValidity',
      'Customer No Show Cancellation Validity (5s threshold for local testing)',
      'ApplicationTag',
      v_bpp_city_id,
      'CustomerNoShowCancellation',
      '{Valid,Invalid}',
      '{"if":[{"and":[{"==":[{"var":"cancellationReason.reasonCode"},"CUSTOMER_NO_SHOW"]},{"==":[{"var":"cancellationReason.source"},"ByDriver"]}]},{"if":[{">=":[{"-":[{"var":"currentTime"},{"if":[{"==":[{"var":"driverArrivalTime"},null]},{"var":"currentTime"},{"var":"driverArrivalTime"}]}]},5]},"Valid","Invalid"]},null]}',
      now(),
      now()
    ) ON CONFLICT (merchant_operating_city_id, name) DO UPDATE SET
      rule_engine = EXCLUDED.rule_engine,
      description = EXCLUDED.description,
      updated_at = now();

    -- BPP: Namma tag trigger (v2) for RideCancel event
    INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger_v2 (event, merchant_operating_city_id, tag_name, created_at, updated_at)
    VALUES ('RideCancel', v_bpp_city_id, 'CustomerNoShowCancellation', now(), now())
    ON CONFLICT (event, merchant_operating_city_id, tag_name) DO UPDATE SET
      updated_at = now();

    -- BPP: Namma tag (v2) for CustomerCancellation validation
    -- Gate for rider-initiated cancellation charges
    INSERT INTO atlas_driver_offer_bpp.namma_tag_v2 (
      category, description, tag_type, merchant_operating_city_id, name,
      tags, rule_engine, created_at, updated_at
    ) VALUES (
      'CustomerCancellationValidity',
      'Customer Cancellation Validity (rider cancels after driver arrived + waited 5s)',
      'ApplicationTag',
      v_bpp_city_id,
      'CustomerCancellation',
      '{Valid,Invalid}',
      '{"if":[{"==":[{"var":"cancellationReason.source"},"ByUser"]},{"if":[{">=":[{"-":[{"var":"currentTime"},{"if":[{"==":[{"var":"driverArrivalTime"},null]},{"var":"currentTime"},{"var":"driverArrivalTime"}]}]},5]},"Valid","Invalid"]},null]}',
      now(),
      now()
    ) ON CONFLICT (merchant_operating_city_id, name) DO UPDATE SET
      rule_engine = EXCLUDED.rule_engine,
      description = EXCLUDED.description,
      updated_at = now();

    -- BPP: Namma tag trigger (v2) for CustomerCancellation on RideCancel event
    INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger_v2 (event, merchant_operating_city_id, tag_name, created_at, updated_at)
    VALUES ('RideCancel', v_bpp_city_id, 'CustomerCancellation', now(), now())
    ON CONFLICT (event, merchant_operating_city_id, tag_name) DO UPDATE SET
      updated_at = now();

    RAISE NOTICE 'BPP Helsinki cancellation fee configured (merchant=%, city=%)', v_bpp_merchant_id, v_bpp_city_id;
  END IF;

  ------------------------------------------------------------
  -- BAP setup
  ------------------------------------------------------------
  IF v_bap_merchant_id IS NULL OR v_bap_city_id IS NULL THEN
    RAISE NOTICE 'BRIDGE_FINLAND Helsinki not found in BAP merchant_operating_city, skipping BAP rider_config setup';
  ELSE
    -- BAP: Enable cancellation charge configs in rider_config
    UPDATE atlas_app.rider_config
    SET valid_cancellation_reason_codes_for_immediate_charge = '{"CUSTOMER_NO_SHOW","COULDNOT_CONNECT_WITH_DRIVER","HIGH_FARE","LONG_ETA","OTHER","SHORT_ETA","WRONG_PICKUP_LOC","FORCED_BY_DRIVER"}'
    WHERE merchant_operating_city_id = v_bap_city_id;

    UPDATE atlas_app.rider_config
    SET settle_cancellation_fee_before_next_ride = true
    WHERE merchant_operating_city_id = v_bap_city_id;

    RAISE NOTICE 'BAP Helsinki rider_config configured (merchant=%, city=%)', v_bap_merchant_id, v_bap_city_id;
  END IF;
END $$;
