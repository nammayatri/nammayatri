CREATE TABLE atlas_driver_offer_bpp.incentive_journey_stats ();

ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN condition_operator text  default 'GTE';
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN condition_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN condition_value integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN current_value integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN journey_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN milestone_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN period_key text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN reward_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN reward_value integer ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey_stats ADD PRIMARY KEY ( id);
