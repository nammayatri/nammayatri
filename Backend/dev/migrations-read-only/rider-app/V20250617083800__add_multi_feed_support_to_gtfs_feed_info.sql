-- V20250617083800__add_multi_feed_support_to_gtfs_feed_info.sql

-- Add new columns to the gtfs_feed_info table to support multiple feeds per provider context.
ALTER TABLE gtfs_feed_info
ADD COLUMN platform_type VARCHAR(255),
ADD COLUMN otp_feed_id TEXT,
ADD COLUMN integrated_bpp_config_id UUID,
ADD COLUMN is_active BOOLEAN DEFAULT TRUE;

-- Add a foreign key constraint to integrated_bpp_config table
ALTER TABLE gtfs_feed_info
ADD CONSTRAINT fk_integrated_bpp_config
FOREIGN KEY (integrated_bpp_config_id)
REFERENCES integrated_bpp_config(id);

-- It's recommended to add an index for the new primary lookup query.
-- The exact columns depend on the final query, but based on the plan, it would be on the keys used in findByProviderKeys.
CREATE INDEX IF NOT EXISTS idx_gtfs_feed_info_provider_keys
ON gtfs_feed_info (merchant_id, merchant_operating_city_id, vehicle_type, platform_type, is_active);

COMMENT ON COLUMN gtfs_feed_info.platform_type IS 'Platform type (e.g., APPLICATION, PARTNER) to distinguish feed configurations.';
COMMENT ON COLUMN gtfs_feed_info.otp_feed_id IS 'The textual Feed ID that OpenTripPlanner uses for this specific data feed.';
COMMENT ON COLUMN gtfs_feed_info.integrated_bpp_config_id IS 'Foreign key to integrated_bpp_config, linking this feed to a specific provider data version.';
COMMENT ON COLUMN gtfs_feed_info.is_active IS 'Flag to enable or disable this feed configuration.';
