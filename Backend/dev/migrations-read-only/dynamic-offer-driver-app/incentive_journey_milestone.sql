CREATE TABLE atlas_driver_offer_bpp.incentive_journey_milestone ();

ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN condition_operator text  default 'GTE';
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN condition_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN condition_value integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN journey_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN "order" integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN reward_config_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN reward_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN reward_value integer ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN pickup_special_location_ids text[]  default NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_milestone ADD COLUMN drop_special_location_ids text[]  default NULL;