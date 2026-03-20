-- Run these only in local, for master / prod, we will need to add using separately
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
  'Cancellation reasons with code and iconUrl',
  'CANCELLATION-REASONS',
  '{"reasons": {"merge": [[{"cat":[{"var":""},{"code":"WAIT_TIME_TOO_LONG"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/clock.svg"}]},{"cat":[{"var":""},{"code":"GOT_ANOTHER_RIDE"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/another_car.svg"}]}], {"if": [{"var": "hasRideAssigned"}, [{"cat":[{"var":""},{"code":"WRONG_PICKUP_LOCATION"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/pickup_location.svg"}]},{"cat":[{"var":""},{"code":"DRIVER_NOT_MOVING"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/bad_car.svg"}]}], [{"cat":[{"var":""},{"code":"WRONG_PICKUP_LOCATION"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/wrong_location.svg"}]},{"cat":[{"var":""},{"code":"DRIVER_UNAVAILABLE"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/bad_car.svg"}]}]]}, {"if": [{"and": [{"var": "hasRideAssigned"}, {"var": "isAirConditioned"}]}, [{"cat":[{"var":""},{"code":"AC_NOT_TURNED_ON"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/snowflake.svg"}]}], []]}, [{"cat":[{"var":""},{"code":"OTHER"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/dislike.svg"}]}]]}}',
  0,
  1,
  now(),
  now()
);

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
  'Cancellation reasons with code and iconUrl (includes DRIVER_DEMANDED_EXTRA)',
  'CANCELLATION-REASONS',
  '{"reasons": {"merge": [[{"cat":[{"var":""},{"code":"WAIT_TIME_TOO_LONG"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/clock.svg"}]},{"cat":[{"var":""},{"code":"GOT_ANOTHER_RIDE"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/another_car.svg"}]}], [{"cat":[{"var":""},{"code":"DRIVER_DEMANDED_EXTRA"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/driver_charging_extra.svg"}]}], {"if": [{"var": "hasRideAssigned"}, [{"cat":[{"var":""},{"code":"WRONG_PICKUP_LOCATION"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/pickup_location.svg"}]},{"cat":[{"var":""},{"code":"DRIVER_NOT_MOVING"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/bad_car.svg"}]}], [{"cat":[{"var":""},{"code":"WRONG_PICKUP_LOCATION"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/wrong_location.svg"}]},{"cat":[{"var":""},{"code":"DRIVER_UNAVAILABLE"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/bad_car.svg"}]}]]}, {"if": [{"and": [{"var": "hasRideAssigned"}, {"var": "isAirConditioned"}]}, [{"cat":[{"var":""},{"code":"AC_NOT_TURNED_ON"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/snowflake.svg"}]}], []]}, [{"cat":[{"var":""},{"code":"OTHER"},{"iconUrl":"https://assets.moving.tech/beckn/cancellation-reasons/dislike.svg"}]}]]}}',
  0,
  2,
  now(),
  now()
);

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
  'Default cancellation reasons with code and iconUrl',
  'Unbounded',
  'namma-yatri-0-0000-0000-00000000city',
  now(),
  now()
);
