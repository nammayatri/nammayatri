ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN document_image_id1 character varying(36) DEFAULT 'no-id' NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN document_image_id2 character varying(36);

ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN document_image_id1 character varying(36) DEFAULT 'no-id' NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN document_image_id2 character varying(36);

ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN document_image_id character varying(36) DEFAULT 'no-id' NOT NULL;