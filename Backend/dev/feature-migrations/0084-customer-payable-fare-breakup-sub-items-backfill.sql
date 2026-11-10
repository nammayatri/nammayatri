UPDATE atlas_driver_offer_bpp.transporter_config tc
SET customer_payable_fare_breakup_sub_items_config = '[
  {"key": "PAYMENT_CHARGE_PAID_BY_CUSTOMER", "node": ["PaymentChargePaidByCustomerComponent"]},
  {"key": "CUSTOMER_CANCELLATION_CHARGE", "node": ["CustomerCancellationChargeComponent"]},
  {"key": "AIRPORT_CONVENIENCE_FEE", "node": ["AirportConvenienceFeeComponent"]},
  {"key": "SERVICE_CHARGE", "node": ["ServiceChargeComponent"]},
  {"key": "TOLL_CHARGES", "node": ["TollChargesComponent"]}
]'::json
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.id = tc.merchant_operating_city_id
  AND moc.merchant_short_id IN ('BRIDGE_CABS_PARTNER', 'BRIDGE_FINLAND_PARTNER', 'BRIDGE_NETHERLANDS_PARTNER', 'MSIL_PARTNER')
  AND tc.customer_payable_fare_breakup_sub_items_config IS NULL;
