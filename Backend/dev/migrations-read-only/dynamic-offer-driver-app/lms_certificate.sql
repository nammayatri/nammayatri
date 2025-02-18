CREATE TABLE atlas_driver_offer_bpp.lms_certificate ();

ALTER TABLE atlas_driver_offer_bpp.lms_certificate ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_certificate ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_certificate ADD COLUMN module_completion_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_certificate ADD COLUMN module_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_certificate ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.lms_certificate ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.lms_certificate ADD PRIMARY KEY ( id);
