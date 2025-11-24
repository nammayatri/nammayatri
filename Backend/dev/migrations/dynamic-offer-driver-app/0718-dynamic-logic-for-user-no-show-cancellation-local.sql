-- ONLY FOR LOCAL
INSERT INTO
  atlas_driver_offer_bpp.app_dynamic_logic_rollout (
    domain,
    percentage_rollout,
    version,
    version_description,
    time_bounds,
    merchant_operating_city_id,
    created_at,
    updated_at
  )
VALUES
  (
    'USER-CANCELLATION-DUES',
    100,
    1,
    'User cancellation dues',
    'Unbounded',
    'favorit0-0000-0000-0000-00000000city',
    now(),
    now()
  );

-- ONLY FOR LOCAL
-- Check all conditions:
-- -- totalBookings >= 3
-- -- cancelledRides >= 2
-- -- serviceTier == "AUTO_RICKSHAW"
-- If ALL conditions are true:
-- -- Set cancellationCharges = 5
-- If ANY condition is false:
-- -- Set cancellationCharges = 0
-- Return all input data + computed cancellationCharges
insert into
  atlas_driver_offer_bpp.app_dynamic_logic_element (
    description,
    domain,
    logic,
    "order",
    merchant_id,
    version
  )
values
  (
    'User cancellation dues generic example',
    'USER-CANCELLATION-DUES',
    '{"cat":[{"var":""},{"if":[{"and":[{">=":[{"var":"totalBookings"},3]},{">=":[{"var":"cancelledRides"},2]},{"==":[{"var":"serviceTier"},"AUTO_RICKSHAW"]}]},{"cancellationCharges":5},{"cancellationCharges":0}]}]}',
    0,
    'favorit0-0000-0000-0000-00000favorit',
    1
  );

-- ONLY FOR LOCAL
-- Check cancellation reason:
-- If "CUSTOMER_NO_SHOW":
-- -- If driver arrived (isArrivedAtPickup = true) AND waited >=240 seconds -> set cancellationCharges = 6
-- -- Else -> set cancellationCharges = 0
-- If other reason:
-- -- Keep existing cancellationCharges value if present
-- -- Else set cancellationCharges = 0 (default)
-- Return all input data + computed cancellationCharges
insert into
  atlas_driver_offer_bpp.app_dynamic_logic_element (
    description,
    domain,
    logic,
    "order",
    merchant_id,
    version
  )
values
  (
    'Customer no show dues example',
    'USER-CANCELLATION-DUES',
    '{"cat":[{"var":""},{"cancellationCharges":{"if":[{"and":[{"==":[{"var":"cancellationReasonSelected"},"CUSTOMER_NO_SHOW"]},{"==":[{"var":"cancelledBy"},"CancellationByDriver"]}]},{"if":[{"and":[{"==":[{"var":"isArrivedAtPickup"},true]},{">=":[{"var":"driverWaitingTime"},240]}]},6,0]},{"if":[{"var":"cancellationCharges"},{"var":"cancellationCharges"},0]}]}}]}',
    1,
    'favorit0-0000-0000-0000-00000favorit',
    1
  );

-- ONLY FOR LOCAL
-- Check if reasonCode = "CUSTOMER_NO_SHOW"
-- If not -> return null
-- If "CUSTOMER_NO_SHOW", check waiting time:
-- -- Calculate: currentTime - driverArrivalTime
-- -- If driverArrivalTime is null -> use currentTime (waiting = 0)
-- -- If waiting time >= 240 seconds -> return "Valid"
-- -- Else -> return "Invalid"
insert into
  atlas_driver_offer_bpp.namma_tag (
    category,
    description,
    chakra,
    event,
    tag_type,
    validity,
    name,
    range_end,
    range_start,
    tags,
    rule_engine,
    created_at,
    updated_at
  )
values
  (
    'CustomerNoShowCancellationValidity',
    'Customer No Show Cancellation Validity',
    null,
    'RideCancel',
    'ApplicationTag',
    null,
    'CustomerNoShowCancellation',
    null,
    null,
    '{Valid,Invalid}',
    '{"if":[{"and":[{"==":[{"var":"cancellationReason.reasonCode"},"CUSTOMER_NO_SHOW"]},{"==":[{"var":"cancellationReason.source"},"ByDriver"]}]},{"if":[{">=":[{"-":[{"var":"currentTime"},{"if":[{"==":[{"var":"driverArrivalTime"},null]},{"var":"currentTime"},{"var":"driverArrivalTime"}]}]},240]},"Valid","Invalid"]},null]}',
    now(),
    now()
  );

-- ONLY FOR LOCAL
INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger (event, tag_name, created_at, updated_at)
VALUES ('RideCancel', 'CustomerNoShowCancellation', now(), now());
