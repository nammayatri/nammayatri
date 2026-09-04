CREATE TABLE atlas_driver_offer_bpp.surge_config ();

ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN apply_on_extra_distance_only boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN created_by text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN max_delta_per_update text ;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN max_multiplier text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN min_multiplier text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN rows text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN time_bounds text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN vehicle_service_tier text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN version integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.surge_config ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.surge_config ADD COLUMN excluded_areas text ;