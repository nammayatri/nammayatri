UPDATE atlas_driver_offer_bpp.transporter_config
SET avg_speed_of_vehicle = (
    avg_speed_of_vehicle::jsonb || '{"suvplus": 40}'::jsonb
);

-- Sync with prod --
UPDATE atlas_driver_offer_bpp.fare_policy_rental_details SET per_hour_charge = 170 where fare_policy_id in (select fare_policy_id from atlas_driver_offer_bpp.fare_product where vehicle_variant = 'AUTO_RICKSHAW' and trip_category = 'Rental_OnDemandStaticOffer' and merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Bangalore', 'Mysore', 'Tumakuru', 'Chennai') or state = 'TamilNadu'));