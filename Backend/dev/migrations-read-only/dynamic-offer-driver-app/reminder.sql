CREATE TABLE atlas_driver_offer_bpp.reminder ();

ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN current_interval_index integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN document_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN due_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN metadata text ;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN reminder_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.reminder ADD PRIMARY KEY ( id);
