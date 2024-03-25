ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN merchant_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id);

UPDATE atlas_driver_offer_bpp.leader_board_configs SET merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN is_enabled BOOLEAN DEFAULT True NOT NULL;

-- Dropping as no longer required
ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN driver_leader_board_expiry;