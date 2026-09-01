-- SWS-5 pickup monitoring for MSIL_PARTNER on the single-clock model (stages/darkStages/afterFaultSec).
-- Distance final rung REALLOCATE_RIDE: ad-hoc reallocates, scheduled is scope-blocked to warn-only.
-- ETA (etaFeasibilityConfig.stages, predicted-lateness-seconds ladder): warn from 1s late, reallocate
-- from 300s late (grace lives in the terminal rung). runBehaviourEngineForScheduled absent = capture-only.
-- COALESCE seeds a full base for NULL rows; jsonb_set(...,true) merges into existing configs.
-- TODO: confirm MSIL cities, tick cadence, rung timings and grace before running.
-- DEPLOY ORDER: run only AFTER all pods run the new binary — REALLOCATE_SCHEDULED_RIDE is a new
-- enum value; old binaries fail to decode the whole pickup_stall_monitoring_config if seeded early.

UPDATE atlas_driver_offer_bpp.transporter_config tc
SET pickup_stall_monitoring_config =
    jsonb_set(
      jsonb_set(
        COALESCE(
          tc.pickup_stall_monitoring_config::jsonb,
          '{"tickIntervalSec":120,"staleFixAfterSec":150,"progressThresholdMeters":50,"stages":[{"afterFaultSec":120,"overlayKey":"PICKUP_STALL_WARN_1","terminalAction":null},{"afterFaultSec":360,"overlayKey":"PICKUP_STALL_WARN_2","terminalAction":null},{"afterFaultSec":600,"overlayKey":"PICKUP_STALL_FINAL","terminalAction":"REALLOCATE_RIDE"}],"darkStages":[{"afterDarkSec":240,"overlayKey":"PICKUP_DARK_1"}]}'::jsonb
        ),
        '{runDistanceMonitorForScheduled}', 'true'::jsonb, true
      ),
      '{etaFeasibilityConfig}',
      '{"stages":[{"afterFaultSec":1,"overlayKey":"SCHEDULED_AT_RISK","terminalAction":null},{"afterFaultSec":300,"overlayKey":"SCHEDULED_AT_RISK","terminalAction":"REALLOCATE_SCHEDULED_RIDE"}]}'::jsonb,
      true
    )::json
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.id = tc.merchant_operating_city_id
  AND moc.merchant_short_id = 'MSIL_PARTNER';
