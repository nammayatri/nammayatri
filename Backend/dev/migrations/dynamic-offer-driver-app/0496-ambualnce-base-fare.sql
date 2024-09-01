ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN base_distance int NOT NULL Default 5000; -- value?

UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 39.24*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco)' and vehicle_age = 0;
UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 29.43*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco)' and vehicle_age = 60;
UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 28.53*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco)' and vehicle_age = 120;

UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 47.25*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco Oxy)' and vehicle_age = 0;
UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 32.49*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco Oxy)' and vehicle_age = 60;
UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 31.14*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance Eco Oxy)' and vehicle_age = 120;

UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 43.6*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance AC)' and vehicle_age = 0;
UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 32.7*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance AC)' and vehicle_age = 60;
UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 31.7*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance AC)' and vehicle_age = 120;

UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 52.5*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance AC Oxy)' and vehicle_age = 0;
UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 36.1*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance AC Oxy)' and vehicle_age = 60;
UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 34.6*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance AC Oxy)' and vehicle_age = 120;

UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 67.4*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance Ventilator)' and vehicle_age = 0;
UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 41.7*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance Ventilator)' and vehicle_age = 60;
UPDATE atlas_driver_offer_bpp.fare_policy set base_fare = 39.4*5 where description = 'Ambulance Fare Policy Kolkata (Ambulance Ventilator)' and vehicle_age = 120;
