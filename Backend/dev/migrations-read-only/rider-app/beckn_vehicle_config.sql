CREATE TABLE atlas_app.beckn_vehicle_config ();

ALTER TABLE atlas_app.beckn_vehicle_config ADD COLUMN beckn_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.beckn_vehicle_config ADD COLUMN black_listed_subscribers text[] NOT NULL;
ALTER TABLE atlas_app.beckn_vehicle_config ADD COLUMN buyer_finder_fee text NOT NULL;
ALTER TABLE atlas_app.beckn_vehicle_config ADD COLUMN category text NOT NULL;
ALTER TABLE atlas_app.beckn_vehicle_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.beckn_vehicle_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.beckn_vehicle_config ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.beckn_vehicle_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.beckn_vehicle_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.beckn_vehicle_config ADD PRIMARY KEY ( id);