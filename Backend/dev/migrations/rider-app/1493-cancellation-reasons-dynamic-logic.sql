-- Migration: Add JsonLogic rules for cancellation reasons (domain: CANCELLATION-REASONS)
--
-- Input (JsonLogic data):
--   { hasRideAssigned: Bool, isAirConditioned: Bool }
--
-- Output:
--   { reasons: [{ code, text, iconUrl }] }
--
-- Versions in this migration:
--   - v1: Base reasons + conditional reasons + (AC reason when applicable) + OTHER
--   - v2: Same as v1, plus DRIVER_DEMANDED_EXTRA
--
-- Rollouts added by this migration:
--   - Local/dev convenience: enables v1 for a single hardcoded merchant_operating_city_id.
--   - Master/prod friendly: enables v2 for merchant operating cities whose city is
--     Bangalore/Bhubaneswar/Puri (matched case-insensitively via lower(moc.city)).
--   - City-scoped enablement is implemented via rollout rows (not via JsonLogic input),
--     since the input does not include city.

INSERT INTO atlas_app.app_dynamic_logic_element (
  description,
  domain,
  logic,
  "order",
  version,
  created_at,
  updated_at
)
VALUES (
  'Cancellation reasons with code, text, and iconUrl',
  'CANCELLATION-REASONS',
  '{"reasons": {"merge": [[{"code":"WAIT_TIME_TOO_LONG","text":"Wait time too long","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/clock.svg"},{"code":"GOT_ANOTHER_RIDE","text":"Got another ride","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/another_car.svg"}], {"if": [{"var": "hasRideAssigned"}, [{"code":"WRONG_PICKUP_LOCATION","text":"Wrong pickup location","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/pickup_location.svg"},{"code":"DRIVER_NOT_MOVING","text":"Driver not moving towards pickup","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/bad_car.svg"}], [{"code":"WRONG_PICKUP_LOCATION","text":"Wrong pickup location","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/wrong_location.svg"},{"code":"DRIVER_UNAVAILABLE","text":"Driver unavailable","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/bad_car.svg"}]]}, {"if": [{"and": [{"var": "hasRideAssigned"}, {"var": "isAirConditioned"}]}, [{"code":"AC_NOT_TURNED_ON","text":"AC not turned on","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/snowflake.svg"}], []]}, [{"code":"OTHER","text":"My issue is not listed here","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/dislike.svg"}]]}}',
  0,
  1,
  now(),
  now()
);

-- Version 2: adds DRIVER_DEMANDED_EXTRA (to be enabled for specific cities via rollout rows).
INSERT INTO atlas_app.app_dynamic_logic_element (
  description,
  domain,
  logic,
  "order",
  version,
  created_at,
  updated_at
)
VALUES (
  'Cancellation reasons with code, text, and iconUrl (includes DRIVER_DEMANDED_EXTRA)',
  'CANCELLATION-REASONS',
  '{"reasons": {"merge": [[{"code":"WAIT_TIME_TOO_LONG","text":"Wait time too long","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/clock.svg"},{"code":"GOT_ANOTHER_RIDE","text":"Got another ride","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/another_car.svg"}], [{"code":"DRIVER_DEMANDED_EXTRA","text":"Driver demanded extra","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/driver_charging_extra.svg"}], {"if": [{"var": "hasRideAssigned"}, [{"code":"WRONG_PICKUP_LOCATION","text":"Wrong pickup location","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/pickup_location.svg"},{"code":"DRIVER_NOT_MOVING","text":"Driver not moving towards pickup","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/bad_car.svg"}], [{"code":"WRONG_PICKUP_LOCATION","text":"Wrong pickup location","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/wrong_location.svg"},{"code":"DRIVER_UNAVAILABLE","text":"Driver unavailable","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/bad_car.svg"}]]}, {"if": [{"and": [{"var": "hasRideAssigned"}, {"var": "isAirConditioned"}]}, [{"code":"AC_NOT_TURNED_ON","text":"AC not turned on","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/snowflake.svg"}], []]}, [{"code":"OTHER","text":"My issue is not listed here","iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/dislike.svg"}]]}}',
  0,
  2,
  now(),
  now()
);

-- Rollout entry to activate the rules for a specific merchantOperatingCityId
-- NOTE: Replace the merchantOperatingCityId below with the actual city IDs
-- where you want cancellation reasons to be active.
-- You need one row per merchantOperatingCityId.

-- Example for local dev (Namma Yatri default city):
INSERT INTO atlas_app.app_dynamic_logic_rollout (
  domain,
  percentage_rollout,
  version,
  version_description,
  time_bounds,
  merchant_operating_city_id,
  created_at,
  updated_at
)
VALUES (
  'CANCELLATION-REASONS',
  100,
  1,
  'Default cancellation reasons with code, text, and iconUrl',
  'Unbounded',
  'namma-yatri-0-0000-0000-00000000city',
  now(),
  now()
);

-- Master/Prod-friendly rollout: enable version 2 for selected operating cities (city names are matched case-insensitively).
-- This implements "visibility.cities" semantics at the rollout layer (since the logic input does not include city).
INSERT INTO atlas_app.app_dynamic_logic_rollout (
  domain,
  percentage_rollout,
  version,
  version_description,
  time_bounds,
  merchant_operating_city_id,
  created_at,
  updated_at
)
SELECT
  'CANCELLATION-REASONS' AS domain,
  100 AS percentage_rollout,
  2 AS version,
  'Cancellation reasons (includes DRIVER_DEMANDED_EXTRA)' AS version_description,
  'Unbounded' AS time_bounds,
  moc.id AS merchant_operating_city_id,
  now() AS created_at,
  now() AS updated_at
FROM atlas_app.merchant_operating_city moc
WHERE lower(moc.city) IN ('bangalore', 'bhubaneswar', 'puri')
;
