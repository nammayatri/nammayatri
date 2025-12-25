CREATE TABLE atlas_driver_offer_bpp.place_name_cache ();

ALTER TABLE atlas_driver_offer_bpp.place_name_cache ADD COLUMN address_components text [] NOT NULL default ARRAY[]::TEXT[];
ALTER TABLE atlas_driver_offer_bpp.place_name_cache ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.place_name_cache ADD COLUMN formatted_address text ;
ALTER TABLE atlas_driver_offer_bpp.place_name_cache ADD COLUMN geo_hash text ;
ALTER TABLE atlas_driver_offer_bpp.place_name_cache ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.place_name_cache ADD COLUMN lat double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.place_name_cache ADD COLUMN lon double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.place_name_cache ADD COLUMN place_id character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.place_name_cache ADD COLUMN plus_code text ;
ALTER TABLE atlas_driver_offer_bpp.place_name_cache ADD PRIMARY KEY ( id);