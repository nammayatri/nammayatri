-- ─── SWS-4 · Overlap / multi-hold ─────────────────────────────────────────────
-- Enables multi-hold for every operating city of merchant 'MSIL_PARTNER'.
-- maxHoldsPerDriver + avgSpeedKmph now live inside the grouped scheduled_ride_config
-- json column (merge preserves the min/max lead keys set by the SWS-2 script);
-- schedule_ride_buffer_time stays a flat column (released, not grouped).
UPDATE atlas_driver_offer_bpp.transporter_config
SET scheduled_ride_config =
      (COALESCE(scheduled_ride_config::jsonb, '{}'::jsonb)
        || jsonb_build_object('maxHoldsPerDriver', 3, 'avgSpeedKmph', 16))::json,  -- 3 = Multi-Hold; avgSpeedKmph deadhead ETA (null => legacy 25)
    schedule_ride_buffer_time = 1800   -- gap between holds + activation lead. NOTE: 1800 (30min) is the legacy default and is sparse for multi-hold; confirm MSIL's intended value
WHERE merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
);
