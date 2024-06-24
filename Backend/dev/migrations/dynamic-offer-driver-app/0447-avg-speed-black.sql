UPDATE atlas_driver_offer_bpp.transporter_config
SET avg_speed_of_vehicle = (
    avg_speed_of_vehicle::jsonb || '{"premiumsedan": 30, "black": 40, "blackxl": 50, "ambulance": 10}'::jsonb
);