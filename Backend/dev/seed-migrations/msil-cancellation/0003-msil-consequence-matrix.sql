-- ============================================================================
-- MSIL cancellation policy on the consequence matrix
-- ============================================================================
-- Replaces the old USER-CANCELLATION-DUES fee rule, which no longer runs.
-- Two halves, per dev/docs/cancellation-consequence-matrix-plan.md:
--
--   1. CANCELLATION-FAULT-VERDICT JsonLogic  -> (atFault, rule)
--      Decides WHETHER and WHY. Thresholds are literals in the rule (no-show wait 300s,
--      grace period 180s, grace distance 300m); retuning them is a rule edit + release.
--
--   2. cancellation_consequence_matrix rows  -> what happens
--      Keyed on the rule name; carries the fee (10% of estimated fare, capped 100).
--
-- Sources: 'Live - ONDC Ride hailing - Ride flow document 11-08-2026.pdf' rider/driver
-- tables, and 'ONDC - Ride Hailing (Nudge Logic).xlsx' sheet 1.
--
-- Idempotent. Safe to re-run.
-- ============================================================================

\set ON_ERROR_STOP on
BEGIN;

-- ---------------------------------------------------------------------------
-- 1. Trust the ONDC cancellation_reason_id the BAP sends, rather than short_desc.
-- ---------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.transporter_config tc
SET prefer_ondc_cancellation_reason_id = true,
    updated_at = NOW()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
JOIN atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
WHERE moc.id = tc.merchant_operating_city_id AND m.short_id = 'MSIL_PARTNER';

-- ---------------------------------------------------------------------------
-- 2. Fault rules, per MSIL operating city.
--    Emits CustomerAtFault + one of three registered rule names, else NoFault.
--    Version is computed as the next free one for the domain: versions are a GLOBAL
--    namespace per domain (findByDomainAndVersion has no merchant filter), so a
--    hardcoded number can silently take over another merchant's rule.
-- ---------------------------------------------------------------------------
DO $$
DECLARE
  v_domain text := 'CANCELLATION-FAULT-VERDICT';
  v_logic  text := $LOGIC${"if":[{"and":[{"==":[{"var":"cancelledBy"},"CancellationByDriver"]},{"==":[{"var":"isArrivedAtPickup"},true]},{"==":[{"var":"cancellationReasonSelected"},"DRIVER_CANCEL_CUSTOMER_NO_SHOW"]},{">=":[{"if":[{"==":[{"var":"driverWaitingTime"},null]},0,{"var":"driverWaitingTime"}]},300]}]},{"cat":[{"var":""},{"atFault":"CustomerAtFault"},{"rule":"customer_no_show"}]},{"if":[{"and":[{"==":[{"var":"cancelledBy"},"CancellationByCustomer"]},{"==":[{"var":"isArrivedAtPickup"},true]},{">":[{"if":[{"==":[{"var":"timeSinceScheduledPickup"},null]},0,{"var":"timeSinceScheduledPickup"}]},0]},{"==":[{"var":"cancellationReasonSelected"},"ONDC_BOOKED_BY_MISTAKE"]}]},{"cat":[{"var":""},{"atFault":"CustomerAtFault"},{"rule":"customer_cancelled_driver_arrived"}]},{"if":[{"and":[{"==":[{"var":"cancelledBy"},"CancellationByCustomer"]},{"==":[{"var":"isArrivedAtPickup"},false]},{">":[{"if":[{"==":[{"var":"timeSinceBooking"},null]},0,{"var":"timeSinceBooking"}]},180]},{">":[{"if":[{"==":[{"var":"actualCoveredDistance"},null]},0,{"var":"actualCoveredDistance"}]},300]},{"==":[{"var":"cancellationReasonSelected"},"ONDC_BOOKED_BY_MISTAKE"]}]},{"cat":[{"var":""},{"atFault":"CustomerAtFault"},{"rule":"customer_late_cancel_driver_moved"}]},{"cat":[{"var":""},{"atFault":"NoFault"},{"rule":"no_fault_default"}]}]}]}]}$LOGIC$;
  v_version int;
  v_city record;
BEGIN
  SELECT COALESCE(MAX(version), 0) + 1 INTO v_version
  FROM atlas_driver_offer_bpp.app_dynamic_logic_element WHERE domain = v_domain;

  INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element
    (description, domain, logic, "order", version, merchant_id, patched_element, created_at, updated_at)
  VALUES ('MSIL cancellation fault verdict - no-show, post-arrival, late-cancel',
          v_domain, v_logic, 0, v_version, NULL, NULL, NOW(), NOW());

  FOR v_city IN
    SELECT moc.id, moc.city, moc.merchant_id FROM atlas_driver_offer_bpp.merchant_operating_city moc
    JOIN atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
    WHERE m.short_id = 'MSIL_PARTNER'
  LOOP
    DELETE FROM atlas_driver_offer_bpp.app_dynamic_logic_rollout
      WHERE merchant_operating_city_id = v_city.id AND domain = v_domain;
    INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_rollout
      (domain, merchant_operating_city_id, percentage_rollout, time_bounds, version,
       version_description, merchant_id, experiment_status, is_base_version, created_at, updated_at)
    VALUES (v_domain, v_city.id, 100, 'Unbounded', v_version,
            'MSIL cancellation fault verdict', v_city.merchant_id, NULL, true, NOW(), NOW());
    RAISE NOTICE 'MSIL fault verdict v% rolled out 100%% to %', v_version, v_city.city;
  END LOOP;
END $$;

COMMIT;

-- ---------------------------------------------------------------------------
-- 3. Matrix rows: what each verdict costs.
--
--    All three scenarios carry the same fee — min(10% x estimated fare, INR 100) —
--    per the sheet. They stay separate rows because the rule name is the most
--    specific dimension (weight 32), so each is independently retunable and
--    independently reportable.
--
--    Tax is 5% of the base; the ALV/GST split stays in Haskell as today.
--    Deleted-then-inserted so a re-run cannot leave two rows on one dimension tuple
--    (direct SQL bypasses the dashboard's duplicate-tuple validation).
-- ---------------------------------------------------------------------------
BEGIN;

DELETE FROM atlas_driver_offer_bpp.cancellation_consequence_matrix ccm
USING atlas_driver_offer_bpp.merchant_operating_city moc,
      atlas_driver_offer_bpp.merchant m
WHERE ccm.merchant_operating_city_id = moc.id
  AND moc.merchant_id = m.id
  AND m.short_id = 'MSIL_PARTNER'
  AND ccm.fault_rule IN ('customer_no_show',
                         'customer_cancelled_driver_arrived',
                         'customer_late_cancel_driver_moved');

INSERT INTO atlas_driver_offer_bpp.cancellation_consequence_matrix
  (id, merchant_id, merchant_operating_city_id,
   fault_verdict, fault_rule, cancelled_by, trip_category, vehicle_service_tier, area, payment_instrument,
   customer_deduction, customer_commission_and_tax, driver_deduction, collection_mode,
   carry_forward_dues, consume_ride_credit_on_cancellation,
   waive_off_allowed, blacklist_driver_for_rider_seconds,
   counts_toward_driver_cancellation_rate, counts_toward_customer_cancellation_stats,
   exempt_dashboard_bookings, driver_notification_key, customer_notification_key, active,
   created_at, updated_at)
SELECT
  md5('msil-ccm:' || moc.id || ':' || r.fault_rule)::uuid::text,
  moc.merchant_id, moc.id,
  'CustomerAtFault', r.fault_rule, r.cancelled_by, NULL, NULL, NULL, NULL,
  '{"tag":"MoneyDeduction","contents":{"tag":"PercentageMoney","percentage":10.0,"minAmount":null,"maxAmount":100.0}}',
  '{"taxPercentage":5.0,"commission":null}',
  NULL, NULL,
  false, false,
  true, NULL,
  false, true,
  true, NULL, NULL, true,
  NOW(), NOW()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
JOIN atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
CROSS JOIN (VALUES
    ('customer_no_show',                 'CancellationByDriver'),
    ('customer_cancelled_driver_arrived', 'CancellationByCustomer'),
    ('customer_late_cancel_driver_moved', 'CancellationByCustomer')
  ) AS r(fault_rule, cancelled_by)
WHERE m.short_id = 'MSIL_PARTNER';

COMMIT;

-- Verify — expect 3 active rows per MSIL operating city:
--
-- SELECT moc.city, ccm.fault_rule, ccm.cancelled_by, ccm.active
-- FROM   atlas_driver_offer_bpp.cancellation_consequence_matrix ccm
-- JOIN   atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = ccm.merchant_operating_city_id
-- JOIN   atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
-- WHERE  m.short_id = 'MSIL_PARTNER' ORDER BY moc.city, ccm.fault_rule;
