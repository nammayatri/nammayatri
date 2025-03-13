CREATE TABLE atlas_app.parcel_details ();

ALTER TABLE atlas_app.parcel_details ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.parcel_details ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.parcel_details ADD COLUMN parcel_type text NOT NULL;
ALTER TABLE atlas_app.parcel_details ADD COLUMN quantity integer ;
ALTER TABLE atlas_app.parcel_details ADD COLUMN search_request_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.parcel_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.parcel_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.parcel_details ADD PRIMARY KEY ( search_request_id);
