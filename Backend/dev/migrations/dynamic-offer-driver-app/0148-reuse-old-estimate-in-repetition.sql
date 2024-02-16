ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN transaction_id character(36);
UPDATE atlas_driver_offer_bpp.driver_quote AS T1 SET transaction_id = (SELECT T2.transaction_id FROM atlas_driver_offer_bpp.search_request AS T2 WHERE T2.id = T1.search_request_id);
ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN transaction_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN transaction_id character(36);
UPDATE atlas_driver_offer_bpp.search_request_for_driver AS T1 SET transaction_id = (SELECT T2.transaction_id FROM atlas_driver_offer_bpp.search_request AS T2 WHERE T2.id = T1.search_request_id);
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN transaction_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN estimate_id character(36);
UPDATE atlas_driver_offer_bpp.search_request SET estimate_id = id;
ALTER TABLE atlas_driver_offer_bpp.search_request ALTER COLUMN estimate_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD CONSTRAINT
  search_request_to_estimate_fk FOREIGN KEY (estimate_id) REFERENCES atlas_driver_offer_bpp.estimate (id);