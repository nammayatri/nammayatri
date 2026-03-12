CREATE TABLE atlas_driver_offer_bpp.monetary_reward_config ();

ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD COLUMN active boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD COLUMN event_function text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD COLUMN event_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD COLUMN expiration_at integer ;
ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD COLUMN monetary_reward_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD COLUMN vehicle_category text ;
ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.monetary_reward_config ADD PRIMARY KEY ( id);
