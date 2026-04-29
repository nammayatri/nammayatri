
-- ALTER TABLE atlas_app.ride ADD CONSTRAINT ride_booking_id_fkey FOREIGN KEY (booking_id) REFERENCES atlas_app.ride_booking(id);

ALTER TABLE atlas_app.ride ADD FOREIGN KEY (merchant_id) REFERENCES atlas_app.merchant(id);

ALTER TABLE atlas_app.ride ADD FOREIGN KEY (merchant_operating_city_id) REFERENCES atlas_app.merchant_operating_city(id);

ALTER TABLE atlas_app.ride OWNER TO atlas_app_user;
