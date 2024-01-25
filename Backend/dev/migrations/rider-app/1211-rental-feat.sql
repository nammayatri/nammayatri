CREATE TABLE atlas_app.rental_details (
    id character(36) NOT NULL PRIMARY KEY,
    base_fare int NOT NULL,
    per_hour_charge int NOT NULL,
    per_extra_min_rate int NOT NULL,
    per_extra_km_rate int NOT NULL,
    night_shift_charge int,
    night_shift_start time without time zone,
    night_shift_end time without time zone
);

ALTER TABLE atlas_app.rental_details OWNER TO atlas_app_user;

