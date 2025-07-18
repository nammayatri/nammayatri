CREATE TABLE atlas_app.multimodal_preferences ();

ALTER TABLE atlas_app.multimodal_preferences ADD COLUMN journey_options_sorting_type text NOT NULL;
ALTER TABLE atlas_app.multimodal_preferences ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multimodal_preferences ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.multimodal_preferences ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.multimodal_preferences ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multimodal_preferences ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multimodal_preferences ADD PRIMARY KEY ( person_id);



------- SQL updates -------

ALTER TABLE atlas_app.multimodal_preferences ADD COLUMN subway_transit_types text[] ;
ALTER TABLE atlas_app.multimodal_preferences ADD COLUMN bus_transit_types text[] ;


------- SQL updates -------

ALTER TABLE atlas_app.multimodal_preferences ADD COLUMN allowed_transit_modes text[] NOT NULL;