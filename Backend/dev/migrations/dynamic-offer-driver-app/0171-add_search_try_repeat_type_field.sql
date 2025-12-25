UPDATE atlas_driver_offer_bpp.search_try AS T1 SET search_repeat_type = 'INITIAL'
  WHERE T1.search_repeat_counter = 0 AND T1.created_at > now () - interval '6 hour';
UPDATE atlas_driver_offer_bpp.search_try AS T1 SET search_repeat_type = 'REALLOCATION'
  WHERE T1.search_repeat_counter IS NULL AND T1.created_at > now () - interval '6 hour';

UPDATE atlas_driver_offer_bpp.search_try AS T1 SET base_fare = (
    SELECT T2.base_fare FROM atlas_driver_offer_bpp.search_request_for_driver AS T2
    WHERE T2.search_try_id = T1.id
    LIMIT 1) -- This select might return null in almost impossible case, need to check with prod and change, if it happened
  WHERE T1.created_at > now () - interval '6 hour';

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN base_fare DROP NOT NULL;

-------------------------------------------------------------------------------------------
-------------------------------AFTER_FULL_ROLL_OUT-----------------------------------------
-------------------------------------------------------------------------------------------

UPDATE atlas_driver_offer_bpp.search_try AS T1 SET search_repeat_type = 'INITIAL' WHERE T1.search_repeat_counter = 0;
UPDATE atlas_driver_offer_bpp.search_try AS T1 SET search_repeat_type = 'REALLOCATION' WHERE T1.search_repeat_counter IS NULL;

UPDATE atlas_driver_offer_bpp.search_try AS T1 SET base_fare = (
    SELECT T2.base_fare FROM atlas_driver_offer_bpp.search_request_for_driver AS T2
    WHERE T2.search_try_id = T1.id
    LIMIT 1) -- This select might return null in almost impossible case, need to check with prod and change, if it happened
  WHERE T1.base_fare IS NULL;

ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN search_repeat_type SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN base_fare SET NOT NULL;