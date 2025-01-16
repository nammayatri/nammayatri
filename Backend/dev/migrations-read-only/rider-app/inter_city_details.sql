CREATE TABLE atlas_app.inter_city_details ();

ALTER TABLE atlas_app.inter_city_details ADD COLUMN base_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN dead_km_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN distance_unit character varying(255) NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN km_per_planned_extra_hour numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN night_shift_charge numeric(30, 2) ;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN night_shift_end time without time zone ;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN night_shift_start time without time zone ;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN per_day_max_hour_allowance int NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN per_extra_km_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN per_extra_min_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN per_hour_charge numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN planned_per_km_rate_one_way numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN planned_per_km_rate_round_trip numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.inter_city_details ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.inter_city_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.inter_city_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;


------- SQL updates -------

ALTER TABLE atlas_app.inter_city_details ADD COLUMN round_trip boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.inter_city_details ADD COLUMN per_day_max_allowance_in_mins int ;