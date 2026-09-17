ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN IF NOT EXISTS enable_ondc_scheduled_ride_support boolean DEFAULT false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN IF NOT EXISTS negotiation_fare_min_tolerance_pct double precision DEFAULT 0.1;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN IF NOT EXISTS negotiation_fare_max_tolerance_pct double precision DEFAULT 0.1;
