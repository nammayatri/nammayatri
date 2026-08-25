-- ─── SWS-4 · Overlap / multi-hold ─────────────────────────────────────────────
-- Enables multi-hold for every operating city of merchant 'MSIL_PARTNER'.
UPDATE atlas_driver_offer_bpp.transporter_config
SET max_scheduled_holds_per_driver = 3,     -- 3 = Multi-Hold
    scheduled_ride_avg_speed_kmph  = 16,    -- deadhead ETA speed; NULL falls back to legacy 25
    schedule_ride_buffer_time      = 1800   -- gap between holds + activation lead. NOTE: 1800 (30min) is the legacy default and is sparse for multi-hold; confirm MSIL's intended value
WHERE merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
);
