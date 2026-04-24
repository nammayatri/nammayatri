-- Helsinki (BRIDGE_FINLAND) — VAT/ServiceVAT config across all fare policies
--
-- Customer VAT 13.5% on all ride components except toll (vat_charge_config)
-- Toll VAT 13.5% on TollChargesComponent (toll_tax_charge_config)
-- ServiceVAT 25.5% on rideFareInclusive (transporter_config.tax_config.serviceVatPercentage)
-- Cancellation customer VAT 13.5% reuses vat_charge_config rate via existing helper
-- Invoice flags: enable VAT breakups, ledger emit, driver invoice line items VAT-inclusive

DO $$
DECLARE
  v_city_id TEXT;
BEGIN
  SELECT moc.id INTO v_city_id
  FROM atlas_driver_offer_bpp.merchant_operating_city moc
  WHERE moc.merchant_short_id = 'BRIDGE_FINLAND_PARTNER' AND moc.city = 'Helsinki'
  LIMIT 1;

  IF v_city_id IS NULL THEN
    RAISE NOTICE 'BRIDGE_FINLAND_PARTNER Helsinki not found in merchant_operating_city, skipping VAT setup';
    RETURN;
  END IF;

  -- 1. FarePolicy: customer VAT 13.5% on all ride components except toll/parking
  --    appliesOn covers ride-fare components across Progressive/Slab/Rental/InterCity/Ambulance fare types.
  UPDATE atlas_driver_offer_bpp.fare_policy
  SET vat_charge_config = '{"value":"13.5%","appliesOn":["RideFare","WaitingCharge","ServiceChargeComponent","CongestionChargeComponent","PetChargeComponent","PriorityChargeComponent","NightShiftChargeComponent","InsuranceChargeComponent","StopChargeComponent","LuggageChargeComponent","CustomerCancellationChargeComponent","CustomerExtraFeeComponent","DeadKmFareComponent","ExtraKmFareComponent","RideDurationFareComponent","TimeBasedFareComponent","DistBasedFareComponent","TimeFareComponent","DistanceFareComponent","PickupChargeComponent","ExtraDistanceFareComponent","ExtraTimeFareComponent","StateEntryPermitChargesComponent","AmbulanceDistBasedFareComponent"]}'
  WHERE merchant_operating_city_id = v_city_id;

  -- 2. FarePolicy: toll VAT 13.5% on TollChargesComponent
  UPDATE atlas_driver_offer_bpp.fare_policy
  SET toll_tax_charge_config = '{"value":"13.5%","appliesOn":["TollChargesComponent"]}'
  WHERE merchant_operating_city_id = v_city_id;

  -- 3. TransporterConfig.taxConfig: ServiceVAT 25.5%. Customer VAT rate is
  --    derived from the ride's fareParams at cancellation time
  --    (projectFareParamsBreakup → tax / taxExclusive), so no separate config.
  UPDATE atlas_driver_offer_bpp.transporter_config
  SET tax_config = jsonb_set(
        COALESCE(tax_config::jsonb, '{}'::jsonb),
        '{serviceVatPercentage}',
        '25.5'::jsonb,
        true
      )::json
  WHERE merchant_operating_city_id = v_city_id;

  -- 4. TransporterConfig.invoiceConfig: VAT-inclusive driver line items.
  UPDATE atlas_driver_offer_bpp.transporter_config
  SET invoice_config = '{"driverInvoiceLineItemsVatInclusive":true}'::json
  WHERE merchant_operating_city_id = v_city_id;

  -- 5. Enable V2 fare calculator so vat_charge_config / toll_tax_charge_config are applied
  UPDATE atlas_driver_offer_bpp.transporter_config
  SET enable_fare_calculator_v2 = true
  WHERE merchant_operating_city_id = v_city_id;

  RAISE NOTICE 'BRIDGE_FINLAND_PARTNER Helsinki VAT config applied for merchant_operating_city_id %', v_city_id;
END $$;

-- Mirror on rider-app side (for customer invoice display flag)
DO $$
DECLARE
  v_city_id TEXT;
BEGIN
  SELECT moc.id INTO v_city_id
  FROM atlas_app.merchant_operating_city moc
  WHERE moc.merchant_short_id = 'BRIDGE_FINLAND' AND moc.city = 'Helsinki'
  LIMIT 1;

  IF v_city_id IS NULL THEN
    RAISE NOTICE 'BRIDGE_FINLAND Helsinki not found in atlas_app.merchant_operating_city, skipping rider invoice config';
    RETURN;
  END IF;

  UPDATE atlas_app.rider_config
  SET invoice_config = '{"ledgerEmitEnabled":true}'::json
  WHERE merchant_operating_city_id = v_city_id;

  RAISE NOTICE 'BRIDGE_FINLAND Helsinki rider invoice config applied for merchant_operating_city_id %', v_city_id;
END $$;
