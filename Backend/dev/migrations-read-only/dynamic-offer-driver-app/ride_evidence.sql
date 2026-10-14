CREATE TABLE atlas_driver_offer_bpp.ride_evidence ();

ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN driver_id character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN id character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN media_file_id character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN merchant_id character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN merchant_operating_city_id character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN ride_id character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN ride_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN hidden_reason text ;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN hidden_by text ;
ALTER TABLE atlas_driver_offer_bpp.ride_evidence ADD COLUMN hidden_at timestamp with time zone ;