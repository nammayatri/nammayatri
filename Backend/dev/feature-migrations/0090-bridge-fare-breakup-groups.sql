-- Fare breakup groups for the dashboard ride-info v2 API (GET /ride/{rideId}/info/v2, `charges`), Bridge only.
--
-- 1. transporter_config.fare_breakup_groups_config: each top-level entry is a group, its entries are the rows,
--    one row per fare piece. Pieces Bridge does not charge (Indian GST / ambulance / interstate permit, and the
--    driver selected / negotiated fare) are left out. GovtChargesComponent is left out because it is the same
--    amount as RideVatComponent, so listing both would count the VAT twice. Rows that are 0 are not sent.
--    Only cities without a value yet are set, as in 0084.
-- 2. English labels, looked up as FARE_BREAKUP_<key>. These keys are new, so they never change the wording of the
--    driver app's ride earnings card (translations are shared by every merchant, keyed by message_key + language).
--    The ADJUSTMENTS group and its rows are built by the API itself, not by the config.
--
-- Rollback:
--   UPDATE atlas_driver_offer_bpp.transporter_config SET fare_breakup_groups_config = NULL
--   WHERE merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
--     WHERE merchant_short_id IN ('BRIDGE_CABS_PARTNER', 'BRIDGE_FINLAND_PARTNER', 'BRIDGE_NETHERLANDS_PARTNER'));
--   DELETE FROM atlas_driver_offer_bpp.translations WHERE message_key LIKE 'FARE_BREAKUP\_%' AND language = 'ENGLISH';

UPDATE atlas_driver_offer_bpp.transporter_config tc
SET fare_breakup_groups_config = '[
  {"key": "PRIMARY_FARE", "node": [
    {"key": "BASE_FARE", "node": ["RideFare"]},
    {"key": "DEAD_KM_FARE", "node": ["DeadKmFareComponent"]},
    {"key": "PICKUP_CHARGE", "node": ["PickupChargeComponent"]},
    {"key": "EXTRA_KM_FARE", "node": ["ExtraKmFareComponent"]},
    {"key": "DISTANCE_FARE", "node": ["DistanceFareComponent"]},
    {"key": "DIST_BASED_FARE", "node": ["DistBasedFareComponent"]},
    {"key": "EXTRA_DISTANCE_FARE", "node": ["ExtraDistanceFareComponent"]},
    {"key": "CONGESTION_CHARGE", "node": ["CongestionChargeComponent"]},
    {"key": "NIGHT_SHIFT_CHARGE", "node": ["NightShiftChargeComponent"]},
    {"key": "WAITING_CHARGE", "node": ["WaitingCharge"]}
  ]},
  {"key": "TIME_BASED", "node": [
    {"key": "RIDE_EXTRA_TIME_FARE", "node": ["RideExtraTimeFareComponent"]},
    {"key": "EXTRA_TIME_FARE", "node": ["ExtraTimeFareComponent"]},
    {"key": "RIDE_DURATION_FARE", "node": ["RideDurationFareComponent"]},
    {"key": "TIME_BASED_FARE", "node": ["TimeBasedFareComponent"]},
    {"key": "TIME_FARE", "node": ["TimeFareComponent"]}
  ]},
  {"key": "TAXES", "node": [
    {"key": "RIDE_VAT", "node": ["RideVatComponent"]},
    {"key": "TOLL_VAT", "node": ["TollVatComponent"]},
    {"key": "PARKING_TAX", "node": ["ParkingTaxComponent"]},
    {"key": "AIRPORT_ENTRY_FEE_TAX", "node": ["AirportEntryFeeTaxComponent"]}
  ]},
  {"key": "ADDITIONAL", "node": [
    {"key": "TOLL_CHARGES", "node": ["TollChargesComponent"]},
    {"key": "PARKING_CHARGE", "node": ["ParkingChargeComponent"]},
    {"key": "SERVICE_CHARGE", "node": ["ServiceChargeComponent"]},
    {"key": "PAYMENT_CHARGE", "node": ["PaymentChargePaidByCustomerComponent"]},
    {"key": "CUSTOMER_EXTRA_FEE", "node": ["CustomerExtraFeeComponent"]},
    {"key": "CANCELLATION_DUES", "node": ["CustomerCancellationChargeComponent"]},
    {"key": "CONDITIONAL_CHARGES", "node": ["ConditionalChargesComponent"]},
    {"key": "AIRPORT_CONVENIENCE_FEE", "node": ["AirportConvenienceFeeComponent"]},
    {"key": "AIRPORT_ENTRY_FEE", "node": ["AirportEntryFeeComponent"]},
    {"key": "BOOTH_CHARGE", "node": ["BoothChargeComponent"]},
    {"key": "CUSTOMER_GATE_FEE", "node": ["CustomerGateFeeComponent"]},
    {"key": "CARD_CHARGE_ON_FARE", "node": ["CardChargeOnFareComponent"]},
    {"key": "CARD_CHARGE_FIXED", "node": ["CardChargeFixedComponent"]},
    {"key": "PET_CHARGE", "node": ["PetChargeComponent"]},
    {"key": "LUGGAGE_CHARGE", "node": ["LuggageChargeComponent"]},
    {"key": "PRIORITY_CHARGE", "node": ["PriorityChargeComponent"]},
    {"key": "INSURANCE_CHARGE", "node": ["InsuranceChargeComponent"]},
    {"key": "ADD_ON_CHARGE", "node": ["AddOnChargeComponent"]},
    {"key": "STOP_CHARGE", "node": ["StopChargeComponent"]},
    {"key": "SCHEDULING_CHARGE", "node": ["SchedulingChargeComponent"]},
    {"key": "DRIVER_ALLOWANCE", "node": ["DriverAllowanceComponent"]},
    {"key": "RETURN_FEE", "node": ["ReturnFeeChargeComponent"]}
  ]}
]'::json
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.id = tc.merchant_operating_city_id
  AND moc.merchant_short_id IN ('BRIDGE_CABS_PARTNER', 'BRIDGE_FINLAND_PARTNER', 'BRIDGE_NETHERLANDS_PARTNER')
  AND tc.fare_breakup_groups_config IS NULL;

INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message, created_at, updated_at)
SELECT md5(t.message_key || '-ENGLISH'), t.message_key, 'ENGLISH', t.message, now(), now()
FROM (VALUES
  ('FARE_BREAKUP_PRIMARY_FARE', 'Primary Fare'),
  ('FARE_BREAKUP_BASE_FARE', 'Base Fare'),
  ('FARE_BREAKUP_DEAD_KM_FARE', 'Dead Km Fare'),
  ('FARE_BREAKUP_PICKUP_CHARGE', 'Pickup Charge'),
  ('FARE_BREAKUP_EXTRA_KM_FARE', 'Extra Km Fare'),
  ('FARE_BREAKUP_DISTANCE_FARE', 'Distance Fare'),
  ('FARE_BREAKUP_DIST_BASED_FARE', 'Distance Based Fare'),
  ('FARE_BREAKUP_EXTRA_DISTANCE_FARE', 'Extra Distance Fare'),
  ('FARE_BREAKUP_CONGESTION_CHARGE', 'Congestion Charge'),
  ('FARE_BREAKUP_NIGHT_SHIFT_CHARGE', 'Night Shift Charge'),
  ('FARE_BREAKUP_WAITING_CHARGE', 'Waiting Charge'),
  ('FARE_BREAKUP_TIME_BASED', 'Time Based Charges'),
  ('FARE_BREAKUP_RIDE_EXTRA_TIME_FARE', 'Extra Time Fare'),
  ('FARE_BREAKUP_EXTRA_TIME_FARE', 'Extra Time Fare'),
  ('FARE_BREAKUP_RIDE_DURATION_FARE', 'Ride Duration Fare'),
  ('FARE_BREAKUP_TIME_BASED_FARE', 'Time Based Fare'),
  ('FARE_BREAKUP_TIME_FARE', 'Time Fare'),
  ('FARE_BREAKUP_TAXES', 'Taxes'),
  ('FARE_BREAKUP_RIDE_VAT', 'Ride VAT'),
  ('FARE_BREAKUP_TOLL_VAT', 'Toll VAT'),
  ('FARE_BREAKUP_PARKING_TAX', 'Parking Tax'),
  ('FARE_BREAKUP_AIRPORT_ENTRY_FEE_TAX', 'Airport Entry Fee Tax'),
  ('FARE_BREAKUP_ADDITIONAL', 'Additional Charges'),
  ('FARE_BREAKUP_TOLL_CHARGES', 'Toll Charges'),
  ('FARE_BREAKUP_PARKING_CHARGE', 'Parking Charge'),
  ('FARE_BREAKUP_SERVICE_CHARGE', 'Service Charge'),
  ('FARE_BREAKUP_PAYMENT_CHARGE', 'Payment Charge'),
  ('FARE_BREAKUP_CUSTOMER_EXTRA_FEE', 'Customer Extra Fee'),
  ('FARE_BREAKUP_CANCELLATION_DUES', 'Cancellation Dues'),
  ('FARE_BREAKUP_CONDITIONAL_CHARGES', 'Conditional Charges'),
  ('FARE_BREAKUP_AIRPORT_CONVENIENCE_FEE', 'Airport Convenience Fee'),
  ('FARE_BREAKUP_AIRPORT_ENTRY_FEE', 'Airport Entry Fee'),
  ('FARE_BREAKUP_BOOTH_CHARGE', 'Booth Charge'),
  ('FARE_BREAKUP_CUSTOMER_GATE_FEE', 'Gate Fee'),
  ('FARE_BREAKUP_CARD_CHARGE_ON_FARE', 'Card Charge On Fare'),
  ('FARE_BREAKUP_CARD_CHARGE_FIXED', 'Card Charge (Fixed)'),
  ('FARE_BREAKUP_PET_CHARGE', 'Pet Charge'),
  ('FARE_BREAKUP_LUGGAGE_CHARGE', 'Luggage Charge'),
  ('FARE_BREAKUP_PRIORITY_CHARGE', 'Priority Charge'),
  ('FARE_BREAKUP_INSURANCE_CHARGE', 'Insurance Charge'),
  ('FARE_BREAKUP_ADD_ON_CHARGE', 'Add-on Charge'),
  ('FARE_BREAKUP_STOP_CHARGE', 'Stop Charge'),
  ('FARE_BREAKUP_SCHEDULING_CHARGE', 'Scheduling Charge'),
  ('FARE_BREAKUP_DRIVER_ALLOWANCE', 'Driver Allowance'),
  ('FARE_BREAKUP_RETURN_FEE', 'Return Fee'),
  ('FARE_BREAKUP_ADJUSTMENTS', 'Adjustments'),
  ('FARE_BREAKUP_DISCOUNT', 'Discount'),
  ('FARE_BREAKUP_COMMISSION', 'Commission'),
  ('FARE_BREAKUP_PERSONAL_DISCOUNT', 'Personal Discount'),
  ('FARE_BREAKUP_BUSINESS_DISCOUNT', 'Business Discount'),
  ('FARE_BREAKUP_NEGATIVE_FARE_ADJUSTMENT', 'Fare Adjustment')
) AS t(message_key, message)
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.translations existing
  WHERE existing.message_key = t.message_key AND existing.language = 'ENGLISH'
);
