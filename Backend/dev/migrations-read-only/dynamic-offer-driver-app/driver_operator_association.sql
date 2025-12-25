CREATE TABLE atlas_driver_offer_bpp.driver_operator_association ();

ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD COLUMN associated_on timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD COLUMN associated_till timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD COLUMN driver_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD COLUMN id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD COLUMN is_active boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD COLUMN operator_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD COLUMN merchant_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_operator_association ADD COLUMN onboarding_vehicle_category text ;