-- -----------------------------------------------------------------------------
-- SWS-1 · Dispatch: dormant-until-T-x + driver-only
-- -----------------------------------------------------------------------------
-- (a) Board OFF — the parked broadcast wave becomes the only accept path.
--     This flag also gates fleet-assign (postDriverFleetScheduledBookingAssign guards
--     on it), so scheduled rides are driver-accept-only via one switch — no separate flag.
UPDATE atlas_driver_offer_bpp.transporter_config tc
SET disable_list_scheduled_booking_api = true
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE tc.merchant_operating_city_id = moc.id
  AND moc.merchant_short_id = 'MSIL_PARTNER';

-- (b) Broadcast wave cadence = MSIL's [x] lead times (seconds, descending).
--     Single wave: '{1800}'  ·  multi-wave example below = T-30 / T-15 / T-5.
--     REPLACE with MSIL's agreed cadence. Must be non-empty (empty => InternalError).
UPDATE atlas_driver_offer_bpp.driver_pool_config dpc
SET schedule_try_times = '{2100}'
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE dpc.merchant_operating_city_id = moc.id
  AND moc.merchant_short_id = 'MSIL_PARTNER';

-- -----------------------------------------------------------------------------
-- SWS-2 · Booking-window validation (BPP defence-in-depth; primary is BAP)
-- -----------------------------------------------------------------------------
-- min/max advance-booking window in SECONDS. Both are TBD by MSIL — values below
-- are placeholders. Leave a column NULL to disable that bound.
-- NOTE: keep minLeadTime >= schedule_ride_buffer_time (default 1800) so a
-- scheduled-classified booking below the floor is rejected, not downgraded to instant.
-- min/max lead now live inside the grouped scheduled_ride_config json column; the
-- merge preserves the other keys (avgSpeedKmph / maxHoldsPerDriver from the SWS-4 script).
UPDATE atlas_driver_offer_bpp.transporter_config tc
SET scheduled_ride_config =
      (COALESCE(tc.scheduled_ride_config::jsonb, '{}'::jsonb)
        || jsonb_build_object('minLeadTime', 1800, 'maxLeadTime', 172800))::json  -- REPLACE min/max lead (seconds) — TBD by MSIL
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE tc.merchant_operating_city_id = moc.id
  AND moc.merchant_short_id = 'MSIL_PARTNER';
