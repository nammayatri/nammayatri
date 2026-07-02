-- Rider cancellation fee: 3-minute rule for Helsinki (BRIDGE_FINLAND / BRIDGE_FINLAND_PARTNER)
--
-- Changes:
--   BPP:
--     1. CustomerCancellation NammaTag rule: charge when rider cancels >= 3 min after booking
--        (tag "CustomerCancellation#Valid" gates the charge path in CancelRide/Internal.hs)
--     2. USER-CANCELLATION-DUES orders 2 & 3 (rider cancellation slots):
--          Order 2: timeSinceBooking >= 180s -> 3 EUR, else 0
--          Order 3: timeSinceBooking >= 180s -> 0.72 EUR tax, else 0
--        Orders 0-1 (CUSTOMER_NO_SHOW) and 4-5 are untouched.
--
-- Deployment order: deploy BPP+BAP code BEFORE running this SQL.
-- Running SQL first would cause currentTime - null(bookingCreatedTime) >= 180 to always
-- evaluate true, charging all riders immediately.

DO $$
DECLARE
  v_bpp_merchant_id TEXT;
  v_bpp_city_id     TEXT;
BEGIN
  SELECT moc.merchant_id, moc.id INTO v_bpp_merchant_id, v_bpp_city_id
  FROM atlas_driver_offer_bpp.merchant_operating_city moc
  WHERE moc.merchant_short_id = 'BRIDGE_FINLAND_PARTNER' AND moc.city = 'Helsinki'
  LIMIT 1;

  IF v_bpp_merchant_id IS NULL OR v_bpp_city_id IS NULL THEN
    RAISE NOTICE 'BRIDGE_FINLAND_PARTNER Helsinki not found, skipping';
    RETURN;
  END IF;

  -- 1. CustomerCancellation NammaTag: fire when rider cancels >= 3 min after booking
  --    Evaluates to "CustomerCancellation#Valid" (validCustomerCancellation in Tools/Constants.hs)
  --    which gates getCancellationCharges in CancelRide/Internal.hs.
  UPDATE atlas_driver_offer_bpp.namma_tag_v2
  SET rule_engine = '{"if":[{"==":[{"var":"cancellationReason.source"},"ByUser"]},{"if":[{">=":[{"-":[{"var":"currentTime"},{"var":"bookingCreatedTime"}]},180]},"Valid","Invalid"]},null]}',
      description = 'Customer Cancellation Validity (rider cancels >= 3 min after booking)',
      updated_at  = now()
  WHERE merchant_operating_city_id = v_bpp_city_id
    AND name = 'CustomerCancellation';

  -- 2. USER-CANCELLATION-DUES: add rider 3-min charge to orders 2 & 3 (version 1)
  --    Orders 0-1 (CUSTOMER_NO_SHOW) are managed by a separate migration and left untouched.

  -- Order 2: Rider 3-min cancellation charge (timeSinceBooking >= 180s -> 3 EUR, else 0)
  UPDATE atlas_driver_offer_bpp.app_dynamic_logic_element
  SET description = 'Rider 3-min cancellation charge',
      logic       = '{"cat":[{"var":""},{"cancellationCharges":{"if":[{"and":[{"==":[{"var":"cancelledBy"},"CancellationByCustomer"]},{">=":[{"var":"timeSinceBooking"},180]}]},3,0]}}]}',
      updated_at  = now()
  WHERE domain = 'USER-CANCELLATION-DUES' AND "order" = 2 AND version = 1
    AND merchant_id = v_bpp_merchant_id;

  -- Order 3: Rider 3-min cancellation tax (same condition -> 0.72 EUR, else 0)
  UPDATE atlas_driver_offer_bpp.app_dynamic_logic_element
  SET description = 'Rider 3-min cancellation tax',
      logic       = '{"cat":[{"var":""},{"cancellationChargesTax":{"if":[{"and":[{"==":[{"var":"cancelledBy"},"CancellationByCustomer"]},{">=":[{"var":"timeSinceBooking"},180]}]},0.72,0]}}]}',
      updated_at  = now()
  WHERE domain = 'USER-CANCELLATION-DUES' AND "order" = 3 AND version = 1
    AND merchant_id = v_bpp_merchant_id;

  RAISE NOTICE 'Rider 3-min cancellation rule applied for Helsinki (merchant=%, city=%)', v_bpp_merchant_id, v_bpp_city_id;
END $$;
