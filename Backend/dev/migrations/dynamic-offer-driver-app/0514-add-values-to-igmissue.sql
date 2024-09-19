ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN rider_id character varying(36);

ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN respondent_name character varying(36);

ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN respondent_email character varying(36);

ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN respondent_phone character varying(36);

ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN responding_merchant_id character varying(36);

ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN respondent_entity_type character varying(36);

ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN transaction_id character varying(36) not null;

ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN merchant_operating_city_id character varying(36);

ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN domain character varying(36) not null;