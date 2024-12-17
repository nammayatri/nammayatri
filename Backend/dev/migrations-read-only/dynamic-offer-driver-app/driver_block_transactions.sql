CREATE TABLE atlas_driver_offer_bpp.driver_block_transactions ();

ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN block_lift_time timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN block_reason text ;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN block_time_in_hours integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN blocked_by text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN reason_code text ;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN reported_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN requestor_id text ;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN block_reason_flag text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_block_transactions ADD COLUMN action_type text ;