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
-- NOTE: keep min_booking_window >= schedule_ride_buffer_time (default 1800) so a
-- scheduled-classified booking below the floor is rejected, not downgraded to instant.
UPDATE atlas_driver_offer_bpp.transporter_config tc
SET min_booking_window = 1800,        -- REPLACE (e.g. 30 min) — TBD by MSIL
    max_booking_window = 172800       -- REPLACE (e.g. 2 days) — TBD by MSIL
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE tc.merchant_operating_city_id = moc.id
  AND moc.merchant_short_id = 'MSIL_PARTNER';
