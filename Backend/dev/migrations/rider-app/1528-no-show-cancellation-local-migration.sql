--LOCAL ONLY
DELETE FROM atlas_app.merchant_service_config
WHERE service_name = 'MultiModalStaticData_OTPTransit';


UPDATE atlas_app.rider_config
SET valid_cancellation_reason_codes_for_immediate_charge = '{"CUSTOMER_NO_SHOW"}'
WHERE merchant_operating_city_id = 'yatri-00-0000-0000-0000-00000000city';

UPDATE atlas_app.rider_config
SET settle_cancellation_fee_before_next_ride = true::boolean
WHERE merchant_operating_city_id = 'yatri-00-0000-0000-0000-00000000city'::varchar(36);