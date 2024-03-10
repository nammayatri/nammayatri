CREATE TABLE atlas_driver_offer_bpp.exophone ();

ALTER TABLE atlas_driver_offer_bpp.exophone ADD COLUMN backup_phone text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.exophone ADD COLUMN call_service text NOT NULL default 'Exotel';
ALTER TABLE atlas_driver_offer_bpp.exophone ADD COLUMN exophone_type text NOT NULL default 'CALL_RIDE';
ALTER TABLE atlas_driver_offer_bpp.exophone ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.exophone ADD COLUMN is_primary_down boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.exophone ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.exophone ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.exophone ADD COLUMN primary_phone text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.exophone ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.exophone ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.exophone ADD PRIMARY KEY ( id, backup_phone, merchant_operating_city_id, primary_phone);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.exophone DROP CONSTRAINT exophone_pkey;
ALTER TABLE atlas_driver_offer_bpp.exophone ADD PRIMARY KEY ( id);