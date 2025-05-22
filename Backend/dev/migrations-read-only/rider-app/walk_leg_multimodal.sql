CREATE TABLE atlas_app.walk_leg_multimodal ();

ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN estimated_distance double precision ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN estimated_duration integer ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN from_location_id character varying(36) ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN agency text ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN convenience_cost integer ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN is_deleted boolean ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN journey_id text ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN journey_leg_order integer ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN pricing_id text ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN skip_booking boolean ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN to_location_id character varying(36) ;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.walk_leg_multimodal ADD PRIMARY KEY ( id);




------- SQL updates -------

ALTER TABLE atlas_app.walk_leg_multimodal ADD COLUMN on_search_failed boolean ;