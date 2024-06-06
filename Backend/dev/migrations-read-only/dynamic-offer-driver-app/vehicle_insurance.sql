CREATE TABLE atlas_driver_offer_bpp.vehicle_insurance ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN document_image_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN insured_name text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN issue_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN limits_of_liability text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN policy_expiry timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN policy_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN policy_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN policy_provider text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN driver_id character varying(36) NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_insurance ADD COLUMN reject_reason text ;