UPDATE atlas_app.rider_config
SET exotel_app_id_mapping = jsonb_set(
  exotel_app_id_mapping::jsonb,
  '{exotelMap, PostRideSafetyCheckAppletID}',
  '"852313"',
  true
);