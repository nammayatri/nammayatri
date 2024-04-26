UPDATE atlas_driver_offer_bpp.transporter_config
SET avg_speed_of_vehicle = '{"sedan": 30, "suv": 30, "hatchback": 0,"autorickshaw": 0, "taxi" : 0, "taxiplus" : 0, "bike" : 25}'::json where merchant_id='favorit0-0000-0000-0000-00000favorit'; -- replace it only for yathri_sathi merchant_id

------------ For Master (ONLY FOR YATHRI SATHI) ------------


UPDATE atlas_driver_offer_bpp.transporter_config
SET avg_speed_of_vehicle = '{"sedan": 30, "suv": 30, "hatchback": 0,"autorickshaw": 0, "taxi" : 0, "taxiplus" : 0, "bike" : 25}'::json where merchant_id='96dd7f78-787e-4a0b-8675-e9e6fe93bb8f';


------- For Production (ONLY FOR YATHRI SATHI) ------------



UPDATE atlas_driver_offer_bpp.transporter_config
SET avg_speed_of_vehicle = '{"sedan": 30, "suv": 30, "hatchback": 0,"autorickshaw": 0, "taxi" : 0, "taxiplus" : 0, "bike" : 25}'::json where merchant_id='d2929b92-8b12-4e21-9efd-d6203940c4c5';

