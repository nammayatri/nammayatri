CREATE TABLE atlas_driver_offer_bpp.fare_policy_change_request ();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN action text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN checked_by text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN fare_product_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN fare_product_snapshot text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN reason text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN remarks text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN requested_by text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_change_request ADD PRIMARY KEY ( id);
