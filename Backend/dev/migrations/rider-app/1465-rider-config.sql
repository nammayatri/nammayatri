update atlas_app.rider_config set settle_cancellation_fee_before_next_ride = false where merchant_operating_city_id in (
    select id from atlas_app.merchant_operating_city where city = 'Bangalore'
);