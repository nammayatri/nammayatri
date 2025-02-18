CREATE TABLE atlas_driver_offer_bpp.driver_module_completion ();

ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN completed_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN completion_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN entities_completed text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN module_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN rating_at_the_time_of_completion double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN started_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD PRIMARY KEY ( completion_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_module_completion ADD COLUMN expiry timestamp with time zone ;