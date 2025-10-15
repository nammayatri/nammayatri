CREATE TABLE atlas_driver_offer_bpp.subscription_transaction ();

ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN amount numeric(30,2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN entity_id text ;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN from_location_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN running_balance numeric(30,2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN status character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN to_location_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN transaction_type character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_transaction ADD COLUMN fleet_owner_id character varying(36) ;