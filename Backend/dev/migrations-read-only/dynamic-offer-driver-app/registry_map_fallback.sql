CREATE TABLE atlas_driver_offer_bpp.registry_map_fallback ();

ALTER TABLE atlas_driver_offer_bpp.registry_map_fallback ADD COLUMN registry_url character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.registry_map_fallback ADD COLUMN subscriber_id character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.registry_map_fallback ADD COLUMN unique_id character (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.registry_map_fallback ADD PRIMARY KEY ( subscriber_id, unique_id);