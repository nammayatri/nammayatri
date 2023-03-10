ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN search_repeat_counter int;
UPDATE atlas_driver_offer_bpp.search_request SET search_repeat_counter = 0;
ALTER TABLE atlas_driver_offer_bpp.search_request ALTER COLUMN search_repeat_counter SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN transaction_id character(36);
UPDATE atlas_driver_offer_bpp.booking AS T1 SET transaction_id = (
  SELECT T3.transaction_id FROM atlas_driver_offer_bpp.driver_quote AS T2
    JOIN atlas_driver_offer_bpp.search_request AS T3
    ON T2.search_request_id = T3.id
  WHERE T2.id = T1.quote_id);
ALTER TABLE atlas_driver_offer_bpp.booking ALTER COLUMN transaction_id SET NOT NULL;