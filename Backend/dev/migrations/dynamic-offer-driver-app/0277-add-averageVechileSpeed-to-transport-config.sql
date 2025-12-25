UPDATE atlas_driver_offer_bpp.transporter_config
SET avg_speed_of_vehicle = '{"sedan": 30, "suv": 30, "hatchback": 0,"autorickshaw": 0, "taxi" : 0, "taxiplus" : 0, "bike": 30}'::json where merchant_id='favorit0-0000-0000-0000-00000favorit'; -- replace it only for yathri_sathi merchant_id
