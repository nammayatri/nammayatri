ALTER TABLE
    atlas_driver_offer_bpp.idfy_verification ADD COLUMN issue_date_on_doc timestamp with time zone;

ALTER TABLE
    atlas_driver_offer_bpp.idfy_verification ADD COLUMN document_number_encrypted character varying(255) NOT NULL;

ALTER TABLE
    atlas_driver_offer_bpp.idfy_verification ADD COLUMN document_number_hash bytea;



ALTER TABLE
    atlas_driver_offer_bpp.idfy_verification ADD COLUMN image_extraction_validation character varying(255) NOT NULL;
