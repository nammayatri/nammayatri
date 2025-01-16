-- DO NOT RUN THE BELOW QUERIES IN PROD UNTIL ASKED
-- WILL WAIT FOR FE TO DO RESPECTIVE CHANGES

-- PLEASE SEE THE ABOVE COMMENTS BEFORE RUNNING
update atlas_app.rider_config SET tracking_short_url_pattern = 'https://nammayatri.in/u?vp={#vp#}&rideId=';

-- PLEASE SEE THE ABOVE COMMENTS BEFORE RUNNING
update atlas_app.rider_config SET tracking_short_url_pattern = 'https://yatrisathi.in/u?vp={#vp#}&rideId='
where merchant_operating_city_id in (
    select id from atlas_app.merchant_operating_city where city = 'Kolkata'
);
