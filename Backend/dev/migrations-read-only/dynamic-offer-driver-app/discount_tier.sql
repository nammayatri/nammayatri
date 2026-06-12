CREATE TABLE atlas_driver_offer_bpp.discount_tier ();

ALTER TABLE atlas_driver_offer_bpp.discount_tier ADD COLUMN discount_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount_tier ADD COLUMN discount_value double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount_tier ADD COLUMN discount_value_type text NOT NULL default 'PERCENTAGE';
ALTER TABLE atlas_driver_offer_bpp.discount_tier ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount_tier ADD COLUMN threshold_value double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount_tier ADD COLUMN tier_order integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount_tier ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.discount_tier ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.discount_tier ADD PRIMARY KEY ( id);
