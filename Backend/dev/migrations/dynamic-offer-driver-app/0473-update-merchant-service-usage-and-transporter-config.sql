UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = jsonb_set(
    config_json::jsonb,
    '{url}',
    '"https://my.exotel.com/juspay2m/exoml/start_voice/"'::jsonb
)::json
WHERE service_name = 'Call_Exotel';

UPDATE atlas_driver_offer_bpp.transporter_config
SET exotel_app_id_mapping = '{"exotelMap":{"SosAppletID": "13775", "RentalAppletID": "13631"}}';