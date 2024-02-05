CREATE TABLE atlas_driver_offer_bpp.merchant_state ();

ALTER TABLE atlas_driver_offer_bpp.merchant_state ADD COLUMN allowed_destination_states text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_state ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_state ADD COLUMN state text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_state ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.merchant_state ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.merchant_state ADD PRIMARY KEY ( merchant_id, state);