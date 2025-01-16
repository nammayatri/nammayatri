CREATE TABLE atlas_driver_offer_bpp.app_dynamic_logic_rollout ();

ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_rollout ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_rollout ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_rollout ADD COLUMN percentage_rollout integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_rollout ADD COLUMN time_bounds text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_rollout ADD COLUMN version integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_rollout ADD COLUMN version_description text ;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_rollout ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_rollout ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_rollout ADD PRIMARY KEY ( domain, merchant_operating_city_id, time_bounds, version);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_rollout ADD COLUMN merchant_id character varying(36) ;