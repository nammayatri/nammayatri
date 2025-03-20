CREATE TABLE atlas_driver_offer_bpp.fleet_csv ();

ALTER TABLE atlas_driver_offer_bpp.fleet_csv ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_csv ADD COLUMN day date NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_csv ADD COLUMN file_path text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_csv ADD COLUMN fleet_owner_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_csv ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_csv ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_csv ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_csv ADD PRIMARY KEY ( day, fleet_owner_id);
