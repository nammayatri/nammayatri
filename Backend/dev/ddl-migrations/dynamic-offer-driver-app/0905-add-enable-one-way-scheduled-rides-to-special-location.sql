-- The special-zone lib maps one special_location table for both apps; only the rider app reads this.
ALTER TABLE atlas_driver_offer_bpp.special_location ADD COLUMN IF NOT EXISTS enable_one_way_scheduled_rides boolean;
