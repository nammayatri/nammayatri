CREATE TABLE atlas_driver_offer_bpp.permit_applicant ();

ALTER TABLE atlas_driver_offer_bpp.permit_applicant ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.permit_applicant ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_applicant ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_applicant ADD COLUMN mobile_country_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_applicant ADD COLUMN mobile_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_applicant ADD COLUMN mobile_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_applicant ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.permit_applicant ADD PRIMARY KEY ( id);
