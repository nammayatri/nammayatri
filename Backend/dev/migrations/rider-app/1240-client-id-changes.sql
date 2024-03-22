ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN client_id character varying(36);

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN client_id character varying(36);

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN client_id character varying(36);