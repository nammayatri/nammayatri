ALTER TABLE atlas_driver_offer_bpp.message_report ADD COLUMN like_status BOOLEAN  NOT NULL DEFAULT false;
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN like_count Int  NOT NULL  DEFAULT 0;
