------Check exact prod config before release

-- Merchant: 7e3d0f76-4c63-4b38-93c4-29b1e508fa21  |  list = {Idfy, HyperVergeRCDL}  (8 cities)
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET category_based_verification_priority_list =
  '{"TOTO_UDIN": ["Tten"], "TOTO": ["Idfy"], "CAR": ["Idfy","HyperVergeRCDL"], "AUTO_CATEGORY": ["Idfy","HyperVergeRCDL"], "MOTORCYCLE": ["Idfy","HyperVergeRCDL"], "TRUCK": ["Idfy","HyperVergeRCDL"], "BOAT": ["Idfy","HyperVergeRCDL"], "AMBULANCE": ["Idfy","HyperVergeRCDL"], "BUS": ["Idfy","HyperVergeRCDL"], "TRAIN": ["Idfy","HyperVergeRCDL"], "FLIGHT": ["Idfy","HyperVergeRCDL"]}'::jsonb
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_id = '7e3d0f76-4c63-4b38-93c4-29b1e508fa21'
    AND city IN ('Lucknow','Chandigarh','Ahmedabad','Surat','Vadodara','Rajkot','Dwarka','Somnath')
);

-- Merchant: 7e3d0f76-4c63-4b38-93c4-29b1e508fa21  |  list = {Morth, Idfy, HyperVergeRCDL}  (5 cities)
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET category_based_verification_priority_list =
  '{"TOTO_UDIN": ["Tten"], "TOTO": ["Idfy"], "CAR": ["Morth","Idfy","HyperVergeRCDL"], "AUTO_CATEGORY": ["Morth","Idfy","HyperVergeRCDL"], "MOTORCYCLE": ["Morth","Idfy","HyperVergeRCDL"], "TRUCK": ["Morth","Idfy","HyperVergeRCDL"], "BOAT": ["Morth","Idfy","HyperVergeRCDL"], "AMBULANCE": ["Morth","Idfy","HyperVergeRCDL"], "BUS": ["Morth","Idfy","HyperVergeRCDL"], "TRAIN": ["Morth","Idfy","HyperVergeRCDL"], "FLIGHT": ["Morth","Idfy","HyperVergeRCDL"]}'::jsonb
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_id = '7e3d0f76-4c63-4b38-93c4-29b1e508fa21'
    AND city IN ('Mumbai','Jaipur','Delhi','Gurugram','Noida')
);

-- Merchant: 7f7896dd-787e-4a0b-8675-e9e6fe93bb8f  |  list = {Idfy, HyperVergeRCDL}  (47 cities)
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET category_based_verification_priority_list = '{"TOTO_UDIN": ["Tten"], "TOTO": ["Idfy"], "CAR": ["Idfy","HyperVergeRCDL"], "AUTO_CATEGORY": ["Idfy","HyperVergeRCDL"], "MOTORCYCLE": ["Idfy","HyperVergeRCDL"], "TRUCK": ["Idfy","HyperVergeRCDL"], "BOAT": ["Idfy","HyperVergeRCDL"], "AMBULANCE": ["Idfy","HyperVergeRCDL"], "BUS": ["Idfy","HyperVergeRCDL"], "TRAIN": ["Idfy","HyperVergeRCDL"], "FLIGHT": ["Idfy","HyperVergeRCDL"]}'::jsonb
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f'
    AND city IN (
      'Bijapur','Belagavi','Ballari','Mayiladuthurai','Nalgonda','Srinagar','Delhi',
      'Jaipur','Bidar','Davanagere','Kochi','Hosur','Noida','Thanjavur','Gulbarga',
      'Cuttack','Chandigarh','Tirupati','Mumbai','Pudukkottai','Tirunelveli','Trichy',
      'Warangal','Jharsuguda','Bhubaneshwar','Gurugram','Madurai','Hyderabad','Minneapolis',
      'Trivandrum','Shivamogga','Sambalpur','Udupi','Vellore','Hubli','Thrissur','Paris',
      'Mysore','Kozhikode','Salem','Puri','TamilNaduCities','Chennai','Pondicherry',
      'Bangalore','Tumakuru','Mangalore'
    )
);

-- Merchant: d2929b92-8b12-4e21-9efd-d6203940c4c5  |  list = {Idfy, HyperVergeRCDL}  (9 cities)
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET category_based_verification_priority_list = '{"TOTO_UDIN": ["Tten"], "TOTO": ["Idfy"], "CAR": ["Idfy","HyperVergeRCDL"], "AUTO_CATEGORY": ["Idfy","HyperVergeRCDL"], "MOTORCYCLE": ["Idfy","HyperVergeRCDL"], "TRUCK": ["Idfy","HyperVergeRCDL"], "BOAT": ["Idfy","HyperVergeRCDL"], "AMBULANCE": ["Idfy","HyperVergeRCDL"], "BUS": ["Idfy","HyperVergeRCDL"], "TRAIN": ["Idfy","HyperVergeRCDL"], "FLIGHT": ["Idfy","HyperVergeRCDL"]}'::jsonb
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_id = 'd2929b92-8b12-4e21-9efd-d6203940c4c5'
    AND city IN ('Bankura','PurbaBardhaman','Birbhum','Digha','Asansol','Petrapole','Siliguri','Durgapur','Kolkata')
);

-- Merchant: b7269e46-933a-40c0-b636-7903d29a31b4  |  list = {Idfy, HyperVergeRCDL}  (2 cities)
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET category_based_verification_priority_list = '{"TOTO_UDIN": ["Tten"], "TOTO": ["Idfy"], "CAR": ["Idfy","HyperVergeRCDL"], "AUTO_CATEGORY": ["Idfy","HyperVergeRCDL"], "MOTORCYCLE": ["Idfy","HyperVergeRCDL"], "TRUCK": ["Idfy","HyperVergeRCDL"], "BOAT": ["Idfy","HyperVergeRCDL"], "AMBULANCE": ["Idfy","HyperVergeRCDL"], "BUS": ["Idfy","HyperVergeRCDL"], "TRAIN": ["Idfy","HyperVergeRCDL"], "FLIGHT": ["Idfy","HyperVergeRCDL"]}'::jsonb
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_id = 'b7269e46-933a-40c0-b636-7903d29a31b4'
    AND city IN ('Amsterdam','Minneapolis')
);

-- Merchant: a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e  |  list = {Idfy, HyperVergeRCDL}  (1 city)
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET category_based_verification_priority_list = '{"TOTO_UDIN": ["Tten"], "TOTO": ["Idfy"], "CAR": ["Idfy","HyperVergeRCDL"], "AUTO_CATEGORY": ["Idfy","HyperVergeRCDL"], "MOTORCYCLE": ["Idfy","HyperVergeRCDL"], "TRUCK": ["Idfy","HyperVergeRCDL"], "BOAT": ["Idfy","HyperVergeRCDL"], "AMBULANCE": ["Idfy","HyperVergeRCDL"], "BUS": ["Idfy","HyperVergeRCDL"], "TRAIN": ["Idfy","HyperVergeRCDL"], "FLIGHT": ["Idfy","HyperVergeRCDL"]}'::jsonb
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_id = 'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e'
    AND city IN ('Helsinki')
);