CREATE TABLE atlas_driver_offer_bpp.person_default_emergency_number ();

ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD COLUMN mobile_country_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD COLUMN mobile_number text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD PRIMARY KEY ( mobile_country_code, person_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD COLUMN mobile_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD COLUMN mobile_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number DROP COLUMN mobile_number;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number DROP CONSTRAINT person_default_emergency_number_pkey;
ALTER TABLE atlas_driver_offer_bpp.person_default_emergency_number ADD PRIMARY KEY ( mobile_country_code, mobile_number_hash, person_id);