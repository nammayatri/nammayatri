
ALTER TABLE atlas_driver_offer_bpp.coin_history
ALTER COLUMN entity_id TYPE character varying(255) USING entity_id::character varying(255);
