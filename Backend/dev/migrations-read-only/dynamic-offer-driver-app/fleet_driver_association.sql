CREATE TABLE atlas_driver_offer_bpp.fleet_driver_association ();

ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN associated_on timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN associated_till timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN driver_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN fleet_owner_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN is_active boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN onboarding_vehicle_category text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN onboarded_operator_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN response_reason text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_driver_association ADD COLUMN request_reason text ;