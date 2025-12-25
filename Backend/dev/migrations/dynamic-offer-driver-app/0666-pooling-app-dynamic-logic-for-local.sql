-- ONLY FOR LOCAL
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
    'Filter unsafe drivers at night',
    'POOLING',
    '{ "filter": [ { "var": "drivers" }, { "!=": [ { "var": "driverPoolResult.driverTags.SafetyCohort" }, "Unsafe" ] } ] }',
    0,
    'favorit0-0000-0000-0000-00000favorit',
    1
  );

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
    'Calculate safetyScore',
    'POOLING',
    '{ "map": [ { "var": "drivers" }, { "cat": [ { "var": "" }, { "if": [ { "==": [ { "var": "driverPoolResult.driverTags.SafetyCohort" }, "Safe" ] }, { "safetyScore": 0 }, { "if": [ { "==": [ { "var": "driverPoolResult.driverTags.SafetyCohort" }, "Problematic" ] }, { "safetyScore": 0.5 }, { "if": [ { "==": [ { "var": "driverPoolResult.driverTags.SafetyCohort" }, "Unsafe" ] }, { "safetyScore": 1 }, { "safetyScore": 0.25 } ]} ] } ] } ] } ] }',
    1,
    'favorit0-0000-0000-0000-00000favorit',
    1
  );

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
    'Calculate overall score',
    'POOLING',
    '{ "map": [ { "var": "drivers" }, { "cat": [ { "var": "" }, { "score": { "+": [ { "*": [ { "/": [ { "var": "actualDistanceToPickup" }, 3000 ] }, 0.5 ] }, { "*": [ { "var": "safetyScore" }, 0.5 ] } ] } } ] } ] }',
    2,
    'favorit0-0000-0000-0000-00000favorit',
    1
  );

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
    'Sort Driver on Score',
    'POOLING',
    '{ "sort": [ { "var": "drivers" }, { "var": "score" } ] }',
    3,
    'favorit0-0000-0000-0000-00000favorit',
    1
  );

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
    'Calculate safetyScore',
    'POOLING',
    '{ "map": [ { "var": "drivers" }, { "cat": [ { "var": "" }, { "if": [ { "==": [ { "var": "driverPoolResult.driverTags.SafetyCohort" }, "Safe" ] }, { "safetyScore": 0 }, { "if": [ { "==": [ { "var": "driverPoolResult.driverTags.SafetyCohort" }, "Problematic" ] }, { "safetyScore": 0.5 }, { "if": [ { "==": [ { "var": "driverPoolResult.driverTags.SafetyCohort" }, "Unsafe" ] }, { "safetyScore": 1 }, { "safetyScore": 0.25 } ]} ] } ] } ] } ] }',
    4,
    'favorit0-0000-0000-0000-00000favorit',
    1
  );

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
    'Calculate overall score',
    'POOLING',
    '{ "map": [ { "var": "drivers" }, { "cat": [ { "var": "" }, { "score": { "+": [ { "*": [ { "/": [ { "var": "actualDistanceToPickup" }, 3000 ] }, 0.5 ] }, { "*": [ { "var": "safetyScore" }, 0.5 ] } ] } } ] } ] }',
    5,
    'favorit0-0000-0000-0000-00000favorit',
    1
  );

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
    'Sort Driver on Score',
    'POOLING',
    '{ "sort": [ { "var": "drivers" }, { "var": "score" } ] }',
    6,
    'favorit0-0000-0000-0000-00000favorit',
    1
  );

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
    'POOLING',
    100,
    1,
    'Driver pool score',
    'Unbounded',
    'favorit0-0000-0000-0000-00000000city',
    now(),
    now()
  );