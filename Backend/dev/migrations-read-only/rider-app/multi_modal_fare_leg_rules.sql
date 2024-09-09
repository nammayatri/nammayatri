CREATE TABLE atlas_app.multi_modal_fare_leg_rules ();

ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN from_time_frame_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN max_dist integer NOT NULL;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN min_dist integer NOT NULL;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN network_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN passenger_type text NOT NULL;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN payment_media text NOT NULL;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN to_time_frame_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_fare_leg_rules ADD PRIMARY KEY ( id);