-- Scheduled-ride ETA-feasibility pickup monitoring for MSIL_PARTNER on the single-clock model.
-- Ad-hoc rides run the distance clock (stages/darkStages, faultSeconds accrual). Scheduled rides run
-- TIME mode: each tick faultSeconds = predicted lateness (deficit) vs the scheduled pickup, judged
-- against graceTimeForScheduledRidePickup (TIME-mode breach threshold, also the SRAOU accept gate;
-- separate integer column, defaults to 300s — set it separately if MSIL needs a different value).
-- Within grace warns once on entry (stages[0].overlayKey -> SCHEDULED_AT_RISK overlay + ScheduledPickupEta#AT_RISK
-- tag); a breach past grace reallocates (#BREACHED) after >= 2 consecutive troubled ticks. In TIME mode only
-- stages[0].overlayKey (the warn copy) is read — grace is the threshold; the modes are mutually exclusive per ride.
-- COALESCE seeds a full base for rows whose config is still NULL, then jsonb_set merges the mode.
-- ETA check runs every tickIntervalSec = 300s (5 min). TODO: confirm MSIL cities, distance rung timings.
-- DEPLOY ORDER: run only AFTER all pods run the new binary — scheduledMonitoringMode / TIME_BASED are new.

UPDATE atlas_driver_offer_bpp.transporter_config tc
SET pickup_stall_monitoring_config =
    jsonb_set(
      COALESCE(
        tc.pickup_stall_monitoring_config::jsonb,
        '{"tickIntervalSec":300,"staleFixAfterSec":150,"progressThresholdMeters":50,"stages":[{"afterFaultSec":0,"overlayKey":"SCHEDULED_AT_RISK","terminalAction":null}],"darkStages":[{"afterDarkSec":240,"overlayKey":"PICKUP_DARK_1"}]}'::jsonb
      ),
      '{scheduledMonitoringMode}', '"TIME_BASED"'::jsonb, true
    )::json
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.id = tc.merchant_operating_city_id
  AND moc.merchant_short_id = 'MSIL_PARTNER';
