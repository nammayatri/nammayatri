-- Cancellation legacy cleanup (dev/docs/cancellation-consequence-matrix-plan.md).
-- Two independent sections with DIFFERENT timing — read before running.

--------------------------------------------------------------------------------
-- 1. Repair preview-polluted fault verdicts. RUN WITH THE DEPLOY.
--
-- Before this build, the soft-cancel PREVIEW (rider opening the cancel screen)
-- persisted a customer-attributed fault verdict on the ride row and cached it in
-- Redis for 1h — typically freezing 'early_customer_cancel' — which a later REAL
-- cancel (even a driver one) then reused. The code now runs previews fully dry-run;
-- this clears the junk the old code left on rides that were never cancelled.
-- (Verdicts on already-CANCELLED rides may also be preview-frozen, but the correct
-- value is unrecoverable — treat pre-fix verdict data as unreliable for analysis.)
UPDATE atlas_driver_offer_bpp.ride
SET cancellation_fault_verdict = NULL, cancellation_fault_rule = NULL
WHERE cancellation_fault_verdict IS NOT NULL
  AND status <> 'CANCELLED';

--------------------------------------------------------------------------------
-- 2. Delete the retired cancellation JsonLogic configs.
-- !!! Removes the one-step BINARY ROLLBACK path (the old build reads these rows).
-- !!! Run ONLY after the consequence matrix has been live and stable in every city.
--
-- LogicDomain values are the custom hyphenated Show forms. The enum constructors
-- stay in code (dashboard listing no longer offers them for new configs).
DELETE FROM atlas_driver_offer_bpp.app_dynamic_logic_element
WHERE domain IN ('USER-CANCELLATION-DUES', 'USER-CANCELLATION-DUES-WAIVE-OFF', 'CANCELLATION-COIN-POLICY');

DELETE FROM atlas_driver_offer_bpp.app_dynamic_logic_rollout
WHERE domain IN ('USER-CANCELLATION-DUES', 'USER-CANCELLATION-DUES-WAIVE-OFF', 'CANCELLATION-COIN-POLICY');

-- coin_config Cancellation rows are DEAD CONFIG now: the coin engine dispatches
-- cancellation coins straight from the consequence matrix (amount, direction, expiry),
-- never consulting coin_config for this event (the direct-wallet mode uses wallet
-- configs and ignores Cancellation entirely). Deactivate for hygiene:
UPDATE atlas_driver_offer_bpp.coin_config
SET active = false
WHERE event_function IN ('BookingCancellation', 'BookingCancellationPenalisaton', 'BookingCancellationCompensation');

-- RideCancel cancellation TAG RULES retired (2026-08-24): the fault verdict is the
-- judgment now — analytics counters and the driver penalty-preview validity read it
-- directly, and the tag computation was deleted from both cancel flows. Remove the
-- 'DriverCancellation', 'CustomerCancellation' and 'CustomerNoShowCancellation' tag
-- definitions per city via the NammaTag dashboard (deletion cascades chakra/rollout
-- references, so prefer the dashboard over raw SQL). Historical ride.ride_tags values
-- are untouched.

-- Legacy per-city amount configs whose FIELDS are now removed from the code
-- (fare_policy.driver_cancellation_penalty_amount, fare_parameters
-- .driver_cancellation_penalty_amount, transporter_config
-- .cancellation_fee_payment_method_exceptions / .driver_rider_blacklist_duration_seconds,
-- ride.cancellation_charges_logic_version): the physical columns are simply no longer
-- read or written — leave them in place; drop them in a later manual DDL window if
-- desired. (0050 reads transporter_config.driver_rider_blacklist_duration_seconds to
-- seed matrix blacklist values, so do not drop that one before 0050 has run.)
