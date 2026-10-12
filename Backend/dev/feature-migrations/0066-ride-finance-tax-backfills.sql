-- tax_config.taxRegime picks the tax the fare calculator applies (V1 and V2) and how ride tax is labelled.
--   GST (unset = GST): tax_config.rideGst is charged on the ride (govtCharges), on toll (tollFareTax) and on
--                      parking (parkingChargeTax), in both fare calculator V1 and V2;
--                      fare_policy.vat_charge_config and toll_tax_charge_config are ignored.
--   VAT: fare_policy.vat_charge_config (ride + parking) and toll_tax_charge_config (toll) are charged;
--        govtCharges holds the ride VAT only; tax_config.rideGst is ignored.
-- Every city that prices with a VAT config today, plus the Bridge partners, is set to VAT so it keeps its VAT.
-- PRICING CHANGE for cities left on GST: toll and parking now carry GST at the rideGst rate, and a
-- toll_tax_charge_config without a vat_charge_config stops applying.

UPDATE atlas_driver_offer_bpp.transporter_config
SET tax_config = jsonb_set(
  COALESCE(tax_config::jsonb, '{}'::jsonb),
  '{taxRegime}',
  '"VAT"'::jsonb
)
WHERE merchant_operating_city_id IN (
    SELECT merchant_operating_city_id FROM atlas_driver_offer_bpp.fare_policy
    WHERE vat_charge_config IS NOT NULL AND merchant_operating_city_id IS NOT NULL
  )
  OR merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id IN ('BRIDGE_FINLAND_PARTNER', 'BRIDGE_NETHERLAND_PARTNER')
  );

-- Ride Finance: component tax is always stored and moved separately; these settings only decide how it
-- is shown. By default (key unset) every component's tax is SPLIT (its own row/line). Each clubTaxIn* list
-- / vatClubRules entry names the components whose tax is shown inside the component's own row instead.
-- Components: RIDE_FARE, TOLL_FARE, CANCELLATION_FARE, PARKING_CHARGE, PAYMENT_CHARGE, COMMISSION,
-- CANCELLATION_COMMISSION, AIRPORT_ENTRY_FEE.
-- The backfills below pin every existing city to current prod output.
--
-- driver_wallet_config.clubTaxInEarnings (driver app ride details -> rideEarnings rows)
--   Prod clubs every component except the payment charge (which is clubbed only when the old
--   clubProjectFareInEarnings list names PAYMENT_CHARGE).
--   UI CHANGE when a component is REMOVED from the list (existing row codes stay; the main row drops to the
--   tax-exclusive amount and a new row code is emitted that the driver app must translate):
--     RIDE_FARE         -> new row RIDE_FARE_VAT (FARE_PAID_BY_CUSTOMER excludes ride tax)
--     TOLL_FARE         -> new row TOLL_VAT (FARE_PAID_BY_CUSTOMER excludes toll tax)
--     PARKING_CHARGE    -> new row PARKING_VAT (FARE_PAID_BY_CUSTOMER excludes parking tax)
--     COMMISSION        -> new row COMMISSION_VAT (COMMISSION shows the tax-exclusive commission)
--     PAYMENT_CHARGE    -> existing rows PAYMENT_CHARGE + PAYMENT_CHARGE_VAT (no new code)
--
-- driver_wallet_config.clubTaxInWalletSummary (driver wallet summary additions/deductions)
--   Toll and parking VAT are now posted to their own ledger entries (TollChargesVAT, ParkingChargesVAT);
--   prod shows toll and parking as one VAT-inclusive row, so both are clubbed. Every other ledger
--   reference is already its own row in prod and stays split.
--   A clubbed component keeps its prod itemReference/itemName (e.g. TollCharges) with the tax folded in.
--   UI CHANGE when TOLL_FARE / PARKING_CHARGE is REMOVED: new rows TollChargesVAT / ParkingChargesVAT appear.
--   UI CHANGE when another component is ADDED: its tax row (e.g. CommissionVAT) disappears into the
--   component row (e.g. Commission).
--
-- invoice_config.vatClubRules (invoice PDF and invoice list, applied at render time)
--   Each rule {issuedToType?, invoiceType?, components} names the components to club; the first matching
--   rule wins and an invoice matching no rule is fully split.
--   Replaces invoice_config.driverInvoiceLineItemsVatInclusive. When it was true prod
--   clubbed ride fare, toll and parking on driver/fleet Ride invoices and the cancellation fee on customer
--   RideCancellation invoices; everything else was split. Those cities get the equivalent rules.
--   UI CHANGE when rules change: clubbed lines use the existing *InclVat descriptionTypes
--   (RideFareInclVat, TollFareInclVat, ParkingChargesInclVat, CancellationFeeInclVat); other clubbed
--   components keep their descriptionType with the tax folded into the line amount.

UPDATE atlas_driver_offer_bpp.transporter_config
SET driver_wallet_config = jsonb_set(
  driver_wallet_config::jsonb,
  '{clubTaxInEarnings}',
  CASE
    WHEN COALESCE(driver_wallet_config::jsonb -> 'clubProjectFareInEarnings', '[]'::jsonb) ? 'PAYMENT_CHARGE'
      THEN '["RIDE_FARE", "TOLL_FARE", "CANCELLATION_FARE", "PARKING_CHARGE", "PAYMENT_CHARGE", "COMMISSION", "CANCELLATION_COMMISSION", "AIRPORT_ENTRY_FEE"]'::jsonb
    ELSE '["RIDE_FARE", "TOLL_FARE", "CANCELLATION_FARE", "PARKING_CHARGE", "COMMISSION", "CANCELLATION_COMMISSION", "AIRPORT_ENTRY_FEE"]'::jsonb
  END
)
WHERE driver_wallet_config IS NOT NULL
  AND NOT (driver_wallet_config::jsonb ? 'clubTaxInEarnings');

UPDATE atlas_driver_offer_bpp.transporter_config
SET driver_wallet_config = jsonb_set(
  driver_wallet_config::jsonb,
  '{clubTaxInWalletSummary}',
  '["TOLL_FARE", "PARKING_CHARGE"]'::jsonb
)
WHERE driver_wallet_config IS NOT NULL
  AND NOT (driver_wallet_config::jsonb ? 'clubTaxInWalletSummary');

UPDATE atlas_driver_offer_bpp.transporter_config
SET invoice_config = jsonb_set(
  invoice_config::jsonb,
  '{vatClubRules}',
  '[{"issuedToType": "DRIVER", "invoiceType": "Ride", "components": ["RIDE_FARE", "TOLL_FARE", "PARKING_CHARGE"]}, {"issuedToType": "FLEET_OWNER", "invoiceType": "Ride", "components": ["RIDE_FARE", "TOLL_FARE", "PARKING_CHARGE"]}, {"issuedToType": "CUSTOMER", "invoiceType": "RideCancellation", "components": ["CANCELLATION_FARE"]}]'::jsonb
)
WHERE invoice_config IS NOT NULL
  AND (invoice_config::jsonb ->> 'driverInvoiceLineItemsVatInclusive')::boolean IS TRUE
  AND NOT (invoice_config::jsonb ? 'vatClubRules');
