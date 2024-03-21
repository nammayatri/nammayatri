
--- run this query before deployment to update the previous data

UPDATE atlas_app.ride
SET driver_mobile_country_code = SUBSTRING(driver_mobile_number, 1, 3),
    driver_mobile_number = SUBSTRING(driver_mobile_number, 4);
