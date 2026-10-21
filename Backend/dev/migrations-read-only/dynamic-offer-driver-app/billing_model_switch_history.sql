CREATE TABLE atlas_driver_offer_bpp.billing_model_switch_history ();

ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD COLUMN acknowledged_unused_credits boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD COLUMN from_model text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD COLUMN requested_by text ;
ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD COLUMN source text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD COLUMN to_model text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.billing_model_switch_history ADD PRIMARY KEY ( id);
