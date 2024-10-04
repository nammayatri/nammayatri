update atlas_app.rider_config SET tracking_short_url_pattern = 'https://nammayatri.in/u?vp={#vp#}&rideId={#rideId#}';

update atlas_app.rider_config SET tracking_short_url_pattern = 'https://yatrisathi.in/u?vp={#vp#}&rideId={#rideId#}'
where merchant_operating_city_id in (
    select id from atlas_app.merchant_operating_city where city = 'Kolkata'
);

update atlas_app.rider_config SET app_url = 'https://yatrisathi.page.link/iSwv'
where merchant_operating_city_id in (
    select id from atlas_app.merchant_operating_city where city = 'Kolkata'
);
