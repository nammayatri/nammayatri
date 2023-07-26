ALTER TABLE atlas_driver_offer_bpp.driver_rc_association ADD COLUMN is_rc_active boolean DEFAULT true, ADD COLUMN is_deleted boolean DEFAULT false;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD entity_id character(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD rc_limit integer DEFAULT 3;
ALTER TABLE atlas_driver_offer_bpp.call_status DROP COLUMN ride_id;