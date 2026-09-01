-- Cancellation Consequence Matrix — registry seed + per-city backfill (NO DDL here; the
-- table/columns come from the NammaDSL generator). See
-- dev/docs/cancellation-consequence-matrix-plan.md.
--
-- JSON encodings (aeson generic TaggedObject, tag field "tag"):
--   ConsequenceDeduction (direction is the CONSTRUCTOR; amounts/counts always POSITIVE):
--     take coins : {"tag":"CoinDeduction","coins":5,"expirySeconds":null}
--     give coins : {"tag":"CoinAddition","coins":50,"expirySeconds":null}
--     charge     : {"tag":"MoneyDeduction","contents":{"tag":"FixedMoney","amount":30.0,"overdueAmount":null}}
--                  {"tag":"MoneyDeduction","contents":{"tag":"PercentageMoney","percentage":10.0,"minAmount":10.0,"maxAmount":60.0}}
--     give money : {"tag":"MoneyAddition","contents":{"tag":"FixedMoney","amount":20.0,"overdueAmount":null}}
--   CommissionAndTax:
--     {"taxPercentage":18.0,"commission":{"tag":"PercentageRate","percentage":5.0}}
--     {"taxPercentage":null,"commission":{"tag":"FixedRate","amount":2.0}}
--
-- Dimension text values: faultVerdict ∈ DriverAtFault|CustomerAtFault|SharedFault|NoFault;
-- cancelledBy ∈ CancellationByDriver|CancellationByCustomer; tripCategory/serviceTier/area
-- use their Haskell Show forms (e.g. 'OneWay OneWayOnDemandDynamicOffer', 'AUTO_RICKSHAW',
-- 'Default'); paymentInstrument uses Show of PaymentInstrument (e.g. 'Cash').
--
-- Additions (CoinAddition/MoneyAddition) GIVE to that party: customer money addition
-- offsets outstanding dues (clamped at 0), driver money addition is a wallet credit,
-- coin addition compensates the driver. Never encode direction with a sign — the
-- dashboard handler rejects non-positive amounts. max_waive_offs_per_period +
-- waive_off_period_days (nullable, omitted below) cap waive-offs per rolling window.

--------------------------------------------------------------------------------
-- 1. GLOBAL fault-rule registry (system_configs; one row, JSON list)
--------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.system_configs (id, config_value)
VALUES
  ('cancellation_fault_rule_registry',
   '[{"name":"pickup_stall","description":"Driver stalled/retreated/went dark en route to pickup","active":true},
     {"name":"customer_no_show","description":"Driver cancelled after arriving near pickup or waiting; customer did not show","active":true},
     {"name":"customer_cancelled_driver_arrived","description":"Customer cancelled after the driver arrived or was waiting at pickup","active":true},
     {"name":"customer_late_cancel_driver_moved","description":"Customer cancelled after the driver covered significant distance toward pickup","active":true},
     {"name":"driver_not_moving_complaint","description":"Customer cancelled citing driver not moving / wait too long, corroborated by signals","active":true},
     {"name":"driver_avoidable_cancel","description":"Driver cancelled without acceptable cause (after the early window)","active":true},
     {"name":"driver_avoidable_cancel_early_moving_toward","description":"Driver cancelled in the early window (20-90s) while making progress toward pickup","active":true},
     {"name":"driver_avoidable_cancel_early_moving_away","description":"Driver cancelled in the early window (20-90s) while moving away from pickup","active":true},
     {"name":"driver_avoidable_cancel_early_stationary","description":"Driver cancelled in the early window (20-90s) while stationary or with unknown location","active":true},
     {"name":"driver_excused_cancel","description":"Driver cancel excused: advance booking, edited pickup/destination, or within 20s","active":true},
     {"name":"early_customer_cancel","description":"Customer cancelled within the free window (30s)","active":true},
     {"name":"no_fault_default","description":"No fault attributed by any rule","active":true}]')
ON CONFLICT (id) DO UPDATE SET config_value = EXCLUDED.config_value;

--------------------------------------------------------------------------------
-- 2. Per-city backfill: legacy-parity defaults for EVERY operating city.
--    Behaviour parity targets: counters/blacklist matched legacy semantics; amounts
--    MUST be reviewed per city (legacy amounts lived in JsonLogic, not derivable in SQL).
--------------------------------------------------------------------------------

-- 2a. City default row (all dimensions wildcard): no charges, but legacy side effects —
--     customer-cancel blacklist TTL from transporter_config, no counters.
INSERT INTO atlas_driver_offer_bpp.cancellation_consequence_matrix
  (id, merchant_id, merchant_operating_city_id,
   fault_verdict, fault_rule, cancelled_by, trip_category, vehicle_service_tier, area, payment_instrument,
   customer_deduction, customer_commission_and_tax, driver_deduction, collection_mode,
   waive_off_allowed, blacklist_driver_for_rider_seconds,
   counts_toward_driver_cancellation_rate, counts_toward_customer_cancellation_stats,
   exempt_dashboard_bookings, driver_notification_key, customer_notification_key, active,
   created_at, updated_at)
SELECT
  md5(random()::text || moc.id)::uuid::text, moc.merchant_id, moc.id,
  NULL, NULL, 'CancellationByCustomer', NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL,
  false, COALESCE(tc.driver_rider_blacklist_duration_seconds, 3600),
  false, false,
  true, NULL, NULL, true,
  now(), now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
LEFT JOIN atlas_driver_offer_bpp.transporter_config tc ON tc.merchant_operating_city_id = moc.id;

-- 2b. CustomerAtFault (no-show on driver cancel): charge template + counters.
--     REVIEW AMOUNTS PER CITY before activating (template inserts as active=false).
INSERT INTO atlas_driver_offer_bpp.cancellation_consequence_matrix
  (id, merchant_id, merchant_operating_city_id,
   fault_verdict, fault_rule, cancelled_by, trip_category, vehicle_service_tier, area, payment_instrument,
   customer_deduction, customer_commission_and_tax, driver_deduction, collection_mode,
   waive_off_allowed, blacklist_driver_for_rider_seconds,
   counts_toward_driver_cancellation_rate, counts_toward_customer_cancellation_stats,
   exempt_dashboard_bookings, driver_notification_key, customer_notification_key, active,
   created_at, updated_at)
SELECT
  md5(random()::text || moc.id)::uuid::text, moc.merchant_id, moc.id,
  'CustomerAtFault', 'customer_no_show', 'CancellationByDriver', NULL, NULL, NULL, NULL,
  '{"tag":"MoneyDeduction","contents":{"tag":"FixedMoney","amount":30.0,"overdueAmount":null}}',
  '{"taxPercentage":null,"commission":null}',
  NULL, 'NextRideDues',
  true, NULL,
  false, true,
  true, NULL, NULL, false,
  now(), now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc;

-- 2c. CustomerAtFault (late cancel by customer): charge template + customer stats counter.
INSERT INTO atlas_driver_offer_bpp.cancellation_consequence_matrix
  (id, merchant_id, merchant_operating_city_id,
   fault_verdict, fault_rule, cancelled_by, trip_category, vehicle_service_tier, area, payment_instrument,
   customer_deduction, customer_commission_and_tax, driver_deduction, collection_mode,
   waive_off_allowed, blacklist_driver_for_rider_seconds,
   counts_toward_driver_cancellation_rate, counts_toward_customer_cancellation_stats,
   exempt_dashboard_bookings, driver_notification_key, customer_notification_key, active,
   created_at, updated_at)
SELECT
  md5(random()::text || moc.id)::uuid::text, moc.merchant_id, moc.id,
  'CustomerAtFault', 'customer_late_cancel', 'CancellationByCustomer', NULL, NULL, NULL, NULL,
  '{"tag":"MoneyDeduction","contents":{"tag":"FixedMoney","amount":30.0,"overdueAmount":null}}',
  '{"taxPercentage":null,"commission":null}',
  NULL, 'NextRideDues',
  true, COALESCE(tc.driver_rider_blacklist_duration_seconds, 3600),
  false, true,
  true, NULL, NULL, false,
  now(), now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
LEFT JOIN atlas_driver_offer_bpp.transporter_config tc ON tc.merchant_operating_city_id = moc.id;

-- 2d. Cash exemption: replaces transporter_config.cancellation_fee_payment_method_exceptions.
--     Backfill ONLY for cities that had Cash in the exception list (adjust WHERE as per env).
INSERT INTO atlas_driver_offer_bpp.cancellation_consequence_matrix
  (id, merchant_id, merchant_operating_city_id,
   fault_verdict, fault_rule, cancelled_by, trip_category, vehicle_service_tier, area, payment_instrument,
   customer_deduction, customer_commission_and_tax, driver_deduction, collection_mode,
   waive_off_allowed, blacklist_driver_for_rider_seconds,
   counts_toward_driver_cancellation_rate, counts_toward_customer_cancellation_stats,
   exempt_dashboard_bookings, driver_notification_key, customer_notification_key, active,
   created_at, updated_at)
SELECT
  md5(random()::text || moc.id)::uuid::text, moc.merchant_id, moc.id,
  'CustomerAtFault', NULL, NULL, NULL, NULL, NULL, 'Cash',
  NULL, NULL, NULL, NULL,
  false, NULL,
  false, true,
  true, NULL, NULL, false,
  now(), now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
JOIN atlas_driver_offer_bpp.transporter_config tc ON tc.merchant_operating_city_id = moc.id
WHERE array_to_string(tc.cancellation_fee_payment_method_exceptions, ',') LIKE '%Cash%';

-- 2e. DriverAtFault: coin penalty backfilled from coin_config's (vestigial) value, plus
--     driver money penalty backfilled from fare_policy.driver_cancellation_penalty_amount.
--     COIN AND MONEY ARE EXCLUSIVE PER ROW — prefer the money penalty where one existed:
--     money rows (from fare policies) are inserted per city where any referenced fare
--     policy carried an amount; otherwise a coin row from coin_config.
INSERT INTO atlas_driver_offer_bpp.cancellation_consequence_matrix
  (id, merchant_id, merchant_operating_city_id,
   fault_verdict, fault_rule, cancelled_by, trip_category, vehicle_service_tier, area, payment_instrument,
   customer_deduction, customer_commission_and_tax, driver_deduction, collection_mode,
   waive_off_allowed, blacklist_driver_for_rider_seconds,
   counts_toward_driver_cancellation_rate, counts_toward_customer_cancellation_stats,
   exempt_dashboard_bookings, driver_notification_key, customer_notification_key, active,
   created_at, updated_at)
SELECT
  md5(random()::text || moc.id)::uuid::text, moc.merchant_id, moc.id,
  'DriverAtFault', NULL, 'CancellationByDriver', NULL, NULL, NULL, NULL,
  NULL, NULL,
  CASE
    WHEN penalty.amount IS NOT NULL
      THEN ('{"tag":"MoneyDeduction","contents":{"tag":"FixedMoney","amount":' || penalty.amount || ',"overdueAmount":null}}')
    WHEN cc.coins IS NOT NULL
      -- legacy coin_config stores the penalty as a NEGATIVE delta; the matrix stores a
      -- positive count with direction in the constructor
      THEN ('{"tag":"CoinDeduction","coins":' || abs(cc.coins) || ',"expirySeconds":' || COALESCE(cc.expiration_at::text, 'null') || '}')
    ELSE NULL
  END,
  NULL,
  false, NULL,
  true, false,
  true, NULL, NULL, false,
  now(), now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
LEFT JOIN LATERAL (
  SELECT max(fp.driver_cancellation_penalty_amount) AS amount
  FROM atlas_driver_offer_bpp.fare_product fprod
  JOIN atlas_driver_offer_bpp.fare_policy fp ON fp.id = fprod.fare_policy_id
  WHERE fprod.merchant_operating_city_id = moc.id
    AND fp.driver_cancellation_penalty_amount IS NOT NULL
) penalty ON true
LEFT JOIN LATERAL (
  SELECT cc0.coins, cc0.expiration_at
  FROM atlas_driver_offer_bpp.coin_config cc0
  WHERE cc0.merchant_opt_city_id = moc.id
    AND cc0.event_function = 'BookingCancellationPenalisaton'
    AND cc0.active = true
  ORDER BY cc0.coins ASC
  LIMIT 1
) cc ON true
WHERE penalty.amount IS NOT NULL OR cc.coins IS NOT NULL;

-- NOTE: rows in 2b/2c/2e are inserted with active=false — review amounts per city on the
-- dashboard (or flip active=true here) before enabling. The matrix is authoritative:
-- cities without ACTIVE rows apply NO cancellation consequences.
