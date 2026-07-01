-- Fix: Chennai transporter_config has search_repeat_limit = 0.
-- checkIfRepeatSearch in SharedLogic/Cancel.hs gates reallocation on:
--   searchTry.searchRepeatCounter < searchRepeatLimit
-- With limit = 0, this is always 0 < 0 = False, so D1's cancel never
-- triggers reallocation to D2. Bangalore works because its limit = 3.
-- Set Chennai to 3 to match Bangalore and allow D2 to receive the ride.
UPDATE atlas_driver_offer_bpp.transporter_config
SET search_repeat_limit = 3
WHERE merchant_operating_city_id IN (
    SELECT moc.id FROM atlas_driver_offer_bpp.merchant_operating_city moc
    WHERE moc.city = 'Chennai'
)
AND search_repeat_limit = 0;
