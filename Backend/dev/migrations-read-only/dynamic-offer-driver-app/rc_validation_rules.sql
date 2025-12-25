CREATE TABLE atlas_driver_offer_bpp.rc_validation_rules ();

ALTER TABLE atlas_driver_offer_bpp.rc_validation_rules ADD COLUMN fuel_type text[] ;
ALTER TABLE atlas_driver_offer_bpp.rc_validation_rules ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rc_validation_rules ADD COLUMN max_vehicle_age integer ;
ALTER TABLE atlas_driver_offer_bpp.rc_validation_rules ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rc_validation_rules ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rc_validation_rules ADD COLUMN vehicle_class text[] ;
ALTER TABLE atlas_driver_offer_bpp.rc_validation_rules ADD COLUMN vehicle_oem text[] ;
ALTER TABLE atlas_driver_offer_bpp.rc_validation_rules ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rc_validation_rules ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rc_validation_rules ADD PRIMARY KEY ( id);
