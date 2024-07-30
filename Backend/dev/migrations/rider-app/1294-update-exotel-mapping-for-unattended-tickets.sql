UPDATE atlas_app.rider_config
SET exotel_app_id_mapping = jsonb_set(
  exotel_app_id_mapping::jsonb,
  '{exotelMap, UnattendedTicketAppletID}',
  '"14404"',
  true
);