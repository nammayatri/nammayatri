CREATE TABLE atlas_driver_offer_bpp.rc_transfer_notice ();

ALTER TABLE atlas_driver_offer_bpp.rc_transfer_notice ADD COLUMN acknowledged boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.rc_transfer_notice ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rc_transfer_notice ADD COLUMN has_other_vehicles boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rc_transfer_notice ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rc_transfer_notice ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rc_transfer_notice ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rc_transfer_notice ADD COLUMN new_driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rc_transfer_notice ADD COLUMN previous_driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rc_transfer_notice ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rc_transfer_notice ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rc_transfer_notice ADD PRIMARY KEY ( id);
