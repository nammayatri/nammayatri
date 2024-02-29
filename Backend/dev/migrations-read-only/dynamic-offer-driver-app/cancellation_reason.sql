CREATE TABLE atlas_driver_offer_bpp.cancellation_reason ();

ALTER TABLE atlas_driver_offer_bpp.cancellation_reason ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_reason ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_reason ADD COLUMN priority integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_reason ADD COLUMN reason_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_reason ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cancellation_reason ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cancellation_reason ADD PRIMARY KEY ( reason_code);