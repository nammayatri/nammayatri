CREATE TABLE atlas_app.rental_details ();

ALTER TABLE atlas_app.rental_details ADD COLUMN base_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.rental_details ADD COLUMN base_fare_amount double precision ;
ALTER TABLE atlas_app.rental_details ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_app.rental_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rental_details ADD COLUMN included_km_per_hr int NOT NULL;
ALTER TABLE atlas_app.rental_details ADD COLUMN night_shift_charge numeric(30, 2) ;
ALTER TABLE atlas_app.rental_details ADD COLUMN night_shift_charge_amount double precision ;
ALTER TABLE atlas_app.rental_details ADD COLUMN night_shift_end time without time zone ;
ALTER TABLE atlas_app.rental_details ADD COLUMN night_shift_start time without time zone ;
ALTER TABLE atlas_app.rental_details ADD COLUMN per_extra_km_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.rental_details ADD COLUMN per_extra_km_rate_amount double precision ;
ALTER TABLE atlas_app.rental_details ADD COLUMN per_extra_min_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.rental_details ADD COLUMN per_extra_min_rate_amount double precision ;
ALTER TABLE atlas_app.rental_details ADD COLUMN per_hour_charge numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.rental_details ADD COLUMN per_hour_charge_amount double precision ;
ALTER TABLE atlas_app.rental_details ADD COLUMN planned_per_km_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.rental_details ADD COLUMN planned_per_km_rate_amount double precision ;
ALTER TABLE atlas_app.rental_details ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.rental_details ADD COLUMN included_distance_per_hr_value double precision ;
ALTER TABLE atlas_app.rental_details ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_app.rental_details ADD COLUMN dead_km_fare numeric(30,2) NOT NULL default 0;