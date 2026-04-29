-- Enable cancellation fee feature for Helsinki (BRIDGE_FINLAND / BRIDGE_FINLAND_PARTNER)
-- BPP merchant_id:               a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e  (BRIDGE_FINLAND_PARTNER)
-- BPP merchant_operating_city_id: beabba6a-c817-43d2-93b2-a916f5cf2ceb  (Helsinki)
-- BAP merchant_id:               b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f  (BRIDGE_FINLAND)
-- BAP merchant_operating_city_id: f9903ef6-f595-428e-b5ac-e8816cbdf979  (Helsinki)

------------------------------------------------------------
-- BPP: Enable cancellation fee in transporter_config
------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.transporter_config
SET can_add_cancellation_fee = true
WHERE merchant_operating_city_id = 'beabba6a-c817-43d2-93b2-a916f5cf2ceb';

------------------------------------------------------------
-- BPP: USER-CANCELLATION-DUES dynamic logic rollout (100%)
------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_rollout (
  domain, percentage_rollout, version, version_description,
  time_bounds, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'USER-CANCELLATION-DUES', 100, 1, 'User cancellation dues for Helsinki',
  'Unbounded', 'beabba6a-c817-43d2-93b2-a916f5cf2ceb', now(), now()
) ON CONFLICT (domain, merchant_operating_city_id, time_bounds, version) DO UPDATE SET
  percentage_rollout = EXCLUDED.percentage_rollout,
  version_description = EXCLUDED.version_description,
  updated_at = now();

------------------------------------------------------------
-- BPP: Dynamic logic elements for cancellation charges
------------------------------------------------------------

-- Order 0: Generic cancellation dues
-- totalBookings >= 3 AND cancelledRides >= 2 AND serviceTier == "AUTO_RICKSHAW" -> charge 5, else 3
INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element (
  description, domain, logic, "order", merchant_id, version
) VALUES (
  'User cancellation dues generic example',
  'USER-CANCELLATION-DUES',
  '{"cat":[{"var":""},{"if":[{"and":[{">=":[{"var":"totalBookings"},3]},{">=":[{"var":"cancelledRides"},2]},{"==":[{"var":"serviceTier"},"AUTO_RICKSHAW"]}]},{"cancellationCharges":5},{"cancellationCharges":3}]}]}',
  0,
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  1
) ON CONFLICT (domain, "order", version) DO UPDATE SET
  description = EXCLUDED.description,
  logic = EXCLUDED.logic,
  merchant_id = EXCLUDED.merchant_id;

-- Order 1: Customer no-show dues (driver cancels with CUSTOMER_NO_SHOW)
-- If driver arrived + waited >= 5s -> charge 100, else 0
INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element (
  description, domain, logic, "order", merchant_id, version
) VALUES (
  'Customer no show dues example',
  'USER-CANCELLATION-DUES',
  '{"cat":[{"var":""},{"cancellationCharges":{"if":[{"and":[{"==":[{"var":"cancellationReasonSelected"},"CUSTOMER_NO_SHOW"]},{"==":[{"var":"cancelledBy"},"CancellationByDriver"]}]},{"if":[{"and":[{"==":[{"var":"isArrivedAtPickup"},true]},{">=":[{"var":"driverWaitingTime"},5]}]},100,0]},{"if":[{"var":"cancellationCharges"},{"var":"cancellationCharges"},0]}]}}]}',
  1,
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  1
) ON CONFLICT (domain, "order", version) DO UPDATE SET
  description = EXCLUDED.description,
  logic = EXCLUDED.logic,
  merchant_id = EXCLUDED.merchant_id;

-- Order 2: Rider cancellation dues (rider cancels, NOT CUSTOMER_NO_SHOW)
-- If driver arrived + waited >= 5s -> keep order0 charges, else 0
INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element (
  description, domain, logic, "order", merchant_id, version
) VALUES (
  'Rider cancellation dues example',
  'USER-CANCELLATION-DUES',
  '{"cat":[{"var":""},{"cancellationCharges":{"if":[{"!=":[{"var":"cancellationReasonSelected"},"CUSTOMER_NO_SHOW"]},{"if":[{"and":[{"==":[{"var":"isArrivedAtPickup"},true]},{">=":[{"var":"driverWaitingTime"},5]}]},{"if":[{"var":"cancellationCharges"},{"var":"cancellationCharges"},0]},0]},{"var":"cancellationCharges"}]}}]}',
  2,
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  1
) ON CONFLICT (domain, "order", version) DO UPDATE SET
  description = EXCLUDED.description,
  logic = EXCLUDED.logic,
  merchant_id = EXCLUDED.merchant_id;

------------------------------------------------------------
-- BPP: Namma tag (v2) for CustomerNoShowCancellation validation
-- App exclusively reads from namma_tag_v2 (city-specific)
------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.namma_tag_v2 (
  category, description, tag_type, merchant_operating_city_id, name,
  tags, rule_engine, created_at, updated_at
) VALUES (
  'CustomerNoShowCancellationValidity',
  'Customer No Show Cancellation Validity (5s threshold for local testing)',
  'ApplicationTag',
  'beabba6a-c817-43d2-93b2-a916f5cf2ceb',
  'CustomerNoShowCancellation',
  '{Valid,Invalid}',
  '{"if":[{"and":[{"==":[{"var":"cancellationReason.reasonCode"},"CUSTOMER_NO_SHOW"]},{"==":[{"var":"cancellationReason.source"},"ByDriver"]}]},{"if":[{">=":[{"-":[{"var":"currentTime"},{"if":[{"==":[{"var":"driverArrivalTime"},null]},{"var":"currentTime"},{"var":"driverArrivalTime"}]}]},5]},"Valid","Invalid"]},null]}',
  now(),
  now()
) ON CONFLICT (merchant_operating_city_id, name) DO UPDATE SET
  rule_engine = EXCLUDED.rule_engine,
  description = EXCLUDED.description,
  updated_at = now();

-- BPP: Namma tag trigger (v2) for RideCancel event
INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger_v2 (event, merchant_operating_city_id, tag_name, created_at, updated_at)
VALUES ('RideCancel', 'beabba6a-c817-43d2-93b2-a916f5cf2ceb', 'CustomerNoShowCancellation', now(), now())
ON CONFLICT (event, merchant_operating_city_id, tag_name) DO UPDATE SET
  updated_at = now();

------------------------------------------------------------
-- BPP: Namma tag (v2) for CustomerCancellation validation
-- Gate for rider-initiated cancellation charges
------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.namma_tag_v2 (
  category, description, tag_type, merchant_operating_city_id, name,
  tags, rule_engine, created_at, updated_at
) VALUES (
  'CustomerCancellationValidity',
  'Customer Cancellation Validity (rider cancels after driver arrived + waited 5s)',
  'ApplicationTag',
  'beabba6a-c817-43d2-93b2-a916f5cf2ceb',
  'CustomerCancellation',
  '{Valid,Invalid}',
  '{"if":[{"==":[{"var":"cancellationReason.source"},"ByUser"]},{"if":[{">=":[{"-":[{"var":"currentTime"},{"if":[{"==":[{"var":"driverArrivalTime"},null]},{"var":"currentTime"},{"var":"driverArrivalTime"}]}]},5]},"Valid","Invalid"]},null]}',
  now(),
  now()
) ON CONFLICT (merchant_operating_city_id, name) DO UPDATE SET
  rule_engine = EXCLUDED.rule_engine,
  description = EXCLUDED.description,
  updated_at = now();

-- BPP: Namma tag trigger (v2) for CustomerCancellation on RideCancel event
INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger_v2 (event, merchant_operating_city_id, tag_name, created_at, updated_at)
VALUES ('RideCancel', 'beabba6a-c817-43d2-93b2-a916f5cf2ceb', 'CustomerCancellation', now(), now())
ON CONFLICT (event, merchant_operating_city_id, tag_name) DO UPDATE SET
  updated_at = now();

------------------------------------------------------------
-- BAP: Enable cancellation charge configs in rider_config
------------------------------------------------------------
UPDATE atlas_app.rider_config
SET valid_cancellation_reason_codes_for_immediate_charge = '{"CUSTOMER_NO_SHOW","COULDNOT_CONNECT_WITH_DRIVER","HIGH_FARE","LONG_ETA","OTHER","SHORT_ETA","WRONG_PICKUP_LOC","FORCED_BY_DRIVER"}'
WHERE merchant_operating_city_id = 'f9903ef6-f595-428e-b5ac-e8816cbdf979';

UPDATE atlas_app.rider_config
SET settle_cancellation_fee_before_next_ride = true
WHERE merchant_operating_city_id = 'f9903ef6-f595-428e-b5ac-e8816cbdf979';
