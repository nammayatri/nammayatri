CREATE TABLE atlas_driver_offer_bpp.app_dynamic_logic_experiment_group ();

ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_experiment_group ADD COLUMN experiment_status text ;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_experiment_group ADD COLUMN group_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_experiment_group ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_experiment_group ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_experiment_group ADD COLUMN modified_by character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_experiment_group ADD COLUMN rollout_percentage integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_experiment_group ADD COLUMN time_bounds text NOT NULL default 'Unbounded';
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_experiment_group ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_experiment_group ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_experiment_group ADD PRIMARY KEY ( group_name, merchant_operating_city_id, time_bounds);
