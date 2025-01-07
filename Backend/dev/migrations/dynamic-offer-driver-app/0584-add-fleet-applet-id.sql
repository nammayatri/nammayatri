UPDATE atlas_driver_offer_bpp.transporter_config
SET exotel_app_id_mapping = '{"exotelMap":{"SosAppletID": "13775", "RentalAppletID": "13631", "FleetAppletID": "13776"}}'; -- replace with actual applet id

UPDATE atlas_driver_offer_bpp.transporter_config SET fleet_alert_threshold = 120;