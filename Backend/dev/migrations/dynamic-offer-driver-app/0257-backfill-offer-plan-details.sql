UPDATE atlas_driver_offer_bpp.driver_fee
SET plan_offer_title = REPLACE(plan_offer_title, 'Enjoy UNLIMITED rides, every day!', 'DAILY UNLIMITED');

UPDATE atlas_driver_offer_bpp.driver_fee
SET plan_offer_title = REPLACE(plan_offer_title, 'Up to a maximum of ₹35 per day', 'DAILY PER RIDE');