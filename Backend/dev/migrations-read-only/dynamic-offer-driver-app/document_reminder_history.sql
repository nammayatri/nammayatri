CREATE TABLE atlas_driver_offer_bpp.document_reminder_history ();

ALTER TABLE atlas_driver_offer_bpp.document_reminder_history ADD COLUMN completion_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_reminder_history ADD COLUMN document_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_reminder_history ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_reminder_history ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_reminder_history ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_reminder_history ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_reminder_history ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_reminder_history ADD COLUMN ride_count_at_completion integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_reminder_history ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.document_reminder_history ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.document_reminder_history ADD PRIMARY KEY ( id);
