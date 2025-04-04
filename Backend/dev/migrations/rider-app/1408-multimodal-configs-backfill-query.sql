INSERT INTO atlas_app.multi_modal_configs (
  id,
  merchant_operating_city_id,
  bus_filter_time_buffer_in_seconds,
  created_at,
  updated_at,
  enable_bus_filtering,
  nearby_driver_search_radius
)
SELECT
  atlas_app.uuid_generate_v4() AS id,
  moc.id AS merchant_operating_city_id,
  300 AS bus_filter_time_buffer_in_seconds,
  now() AS created_at,
  now() AS updated_at,
  false AS enable_bus_filtering,
  1 AS nearby_driver_search_radius
FROM
  atlas_app.merchant_operating_city moc;

UPDATE atlas_app.multi_modal_configs mmc
SET
  metro_booking_allowed = rc.metro_booking_allowed,
  maximum_walk_distance = rc.maximum_walk_distance,
  permissible_modes = rc.permissible_modes,
  multimodal_testing = rc.multimodal_testing,
  minimum_walk_distance = rc.minimum_walk_distance,
  max_allowed_public_transport_legs = rc.max_allowed_public_transport_legs,
  make_multi_modal_search = rc.make_multi_modal_search
  timeout_seconds = rc.timeout_seconds
FROM atlas_app.rider_config rc
WHERE rc.merchant_operating_city_id = mmc.merchant_operating_city_id;
