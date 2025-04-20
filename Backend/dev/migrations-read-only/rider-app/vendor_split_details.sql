CREATE TABLE atlas_app.vendor_split_details ();

ALTER TABLE atlas_app.vendor_split_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.vendor_split_details ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.vendor_split_details ADD COLUMN split_type text NOT NULL;
ALTER TABLE atlas_app.vendor_split_details ADD COLUMN vendor_id text NOT NULL;
ALTER TABLE atlas_app.vendor_split_details ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.vendor_split_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.vendor_split_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.vendor_split_details ADD PRIMARY KEY ( id);
