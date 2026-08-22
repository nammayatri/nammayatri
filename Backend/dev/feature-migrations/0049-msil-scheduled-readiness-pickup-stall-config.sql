-- SWS-5 scheduled-readiness knobs for MSIL_PARTNER. Reallocation owner enum is gone: the terminal action
-- scopes reallocation. Distance stages stay REALLOCATE_RIDE (ad-hoc reallocates, scheduled only warns);
-- ETA owns scheduled reallocation via etaFeasibilityConfig head-stage terminalAction=REALLOCATE_SCHEDULED_RIDE.
-- COALESCE seeds a full pickup-stall base mirroring NAMMA_YATRI_PARTNER prod for NULL rows; jsonb_set(...,true) merges.
-- TODO: confirm MSIL cities, the ETA overlay key, and grace_time_for_scheduled_ride_pickup.

UPDATE atlas_driver_offer_bpp.transporter_config tc
SET pickup_stall_monitoring_config =
    jsonb_set(
      jsonb_set(
        COALESCE(
          tc.pickup_stall_monitoring_config::jsonb,
          '{"stalledConfig":{"stages":[{"overlayKey":"PICKUP_STALL_WARN_1","afterStallSec":120,"terminalAction":null},{"overlayKey":"PICKUP_STALL_WARN_1","afterStallSec":300,"terminalAction":null},{"overlayKey":"PICKUP_STALL_FINAL","afterStallSec":480,"terminalAction":"REALLOCATE_RIDE"}]},"retreatingConfig":{"stages":[{"overlayKey":"PICKUP_STALL_WARN_MOVING_AWAY","afterStallSec":30,"terminalAction":null},{"overlayKey":"PICKUP_STALL_FINAL_MA","afterStallSec":90,"terminalAction":"REALLOCATE_RIDE"}]},"locationDarkConfig":{"stages":[{"overlayKey":"PICKUP_STALL_WARN_1","afterStallSec":180,"terminalAction":null},{"overlayKey":"PICKUP_STALL_FINAL","afterStallSec":420,"terminalAction":"REALLOCATE_RIDE"}]},"badTickDebounce":2,"gracePeriodSec":60,"progressThresholdMeters":50,"tickIntervalSec":30}'::jsonb
        ),
        '{runDistanceMonitorForScheduled}', 'true'::jsonb, true
      ),
      '{etaFeasibilityConfig}',
      '{"stages":[{"afterStallSec":0,"overlayKey":"SCHEDULED_AT_RISK","terminalAction":"REALLOCATE_SCHEDULED_RIDE"}]}'::jsonb,
      true
    )::json
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.id = tc.merchant_operating_city_id
  AND moc.merchant_short_id = 'MSIL_PARTNER';
