update atlas_app.rider_config SET app_url = 'https://yatrisathi.page.link/iSwv'
where merchant_operating_city_id in (
    select id from atlas_app.merchant_operating_city where city = 'Kolkata'
);
