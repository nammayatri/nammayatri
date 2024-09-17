CREATE TABLE atlas_app.multi_modal_stops ();

ALTER TABLE atlas_app.multi_modal_stops ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_stops ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.multi_modal_stops ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.multi_modal_stops ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.multi_modal_stops ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_stops ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_stops ADD PRIMARY KEY ( id);