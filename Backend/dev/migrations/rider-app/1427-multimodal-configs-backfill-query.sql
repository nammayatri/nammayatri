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