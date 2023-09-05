ALTER TABLE atlas_driver_offer_bpp.registry_map_fallback ALTER COLUMN subscriber_id TYPE character varying(255);
ALTER TABLE atlas_driver_offer_bpp.registry_map_fallback ALTER COLUMN subscriber_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.registry_map_fallback ALTER COLUMN unique_id TYPE character varying(36);
ALTER TABLE atlas_driver_offer_bpp.registry_map_fallback ALTER COLUMN unique_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.registry_map_fallback ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL