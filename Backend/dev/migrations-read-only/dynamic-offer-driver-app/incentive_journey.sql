CREATE TABLE atlas_driver_offer_bpp.incentive_journey ();

ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN driver_tag text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN end_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN start_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN time_bounds text ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN vehicle_category text ;
ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN journey_type text  default 'Daily';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.incentive_journey ADD COLUMN vehicle_variant text ;