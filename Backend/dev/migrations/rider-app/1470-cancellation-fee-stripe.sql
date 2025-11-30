-- Run for International track in MASTER and PROD
update atlas_app.rider_config set settle_cancellation_fee_before_next_ride = true where merchant_operating_city_id in (
    select id from atlas_app.merchant_operating_city where city in ('Amsterdam', 'Helsinki')
);