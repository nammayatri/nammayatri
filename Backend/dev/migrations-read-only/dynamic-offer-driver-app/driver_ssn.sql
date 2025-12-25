CREATE TABLE atlas_driver_offer_bpp.driver_ssn ();

ALTER TABLE atlas_driver_offer_bpp.driver_ssn ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_ssn ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_ssn ADD COLUMN ssn_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_ssn ADD COLUMN ssn_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_ssn ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_ssn ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_ssn ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_ssn ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_ssn ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_ssn DROP COLUMN updated_at;
ALTER TABLE atlas_driver_offer_bpp.driver_ssn DROP COLUMN merchant_operating_city_id;
ALTER TABLE atlas_driver_offer_bpp.driver_ssn DROP COLUMN merchant_id;
ALTER TABLE atlas_driver_offer_bpp.driver_ssn DROP COLUMN created_at;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_ssn ADD COLUMN verification_status text NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_ssn ADD COLUMN reject_reason text ;