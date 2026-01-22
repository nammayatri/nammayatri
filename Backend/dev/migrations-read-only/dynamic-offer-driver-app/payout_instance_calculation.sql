CREATE TABLE atlas_driver_offer_bpp.payout_instance_calculation ();

ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD COLUMN end_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD COLUMN from_vendor_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD COLUMN instance_balance double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD COLUMN to_vendor_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD PRIMARY KEY ( from_vendor_id, id, to_vendor_id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation DROP CONSTRAINT payout_instance_calculation_pkey;
ALTER TABLE atlas_driver_offer_bpp.payout_instance_calculation ADD PRIMARY KEY ( end_time, from_vendor_id, id, start_time, to_vendor_id);