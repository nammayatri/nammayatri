CREATE TABLE atlas_driver_offer_bpp.incentive_journey ();

ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN journey_type text  default 'Daily';
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN reward_value integer ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN reward_type text ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN reward_expiration_at integer ;

ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN max_waive_off_count integer  default 1;
