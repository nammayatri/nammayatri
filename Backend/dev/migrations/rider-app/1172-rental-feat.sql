CREATE TABLE atlas_app.rental_details (
    id character(36) NOT NULL PRIMARY KEY,
    base_fare int NOT NULL,
    per_hour_charge int NOT NULL,
    per_hour_free_kms int NOT NULL,
    per_extra_km_rate int NOT NULL,
    night_shift_charge int,
    night_shift_start time without time zone,
    night_shift_end time without time zone
);

ALTER TABLE atlas_app.rental_details OWNER TO atlas_app_user;

ALTER TABLE atlas_app.quote RENAME COLUMN rental_slab_id TO rental_details_id;
ALTER TABLE atlas_app.booking RENAME COLUMN rental_slab_id TO rental_details_id;

ALTER TABLE atlas_app.booking ADD COLUMN rental_base_duration bigint;

ALTER TABLE atlas_app.ride ADD COLUMN end_ride_otp char(4);
ALTER TABLE atlas_app.search_request ADD COLUMN tag character varying(36) default 'ON_DEMAND';
