ALTER TABLE atlas_driver_offer_bpp.transporter_config
ADD COLUMN avg_speed_of_vehicle JSON;

UPDATE atlas_driver_offer_bpp.transporter_config
SET avg_speed_of_vehicle = '{"sedan": 30, "suv": 30, "hatchback": 0,"autorickshaw": 0, "taxi" : 0, "taxiplus" : 0}'::json where merchant_id='7f7896dd-787e-4a0b-8675-e9e6fe93bb8f'; -- replace it only for yathri_sathi merchant_id
