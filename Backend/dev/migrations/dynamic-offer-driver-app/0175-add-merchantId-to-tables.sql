ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN merchant_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id);

UPDATE atlas_driver_offer_bpp.booking_cancellation_reason SET merchant_id = NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN merchant_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id);

UPDATE atlas_driver_offer_bpp.search_request_for_driver SET merchant_id = NULL;

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN merchant_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id);

UPDATE atlas_driver_offer_bpp.ride SET merchant_id = NULL;


ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN merchant_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id);

UPDATE atlas_driver_offer_bpp.driver_information SET merchant_id = NULL;


ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN merchant_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id);

UPDATE atlas_driver_offer_bpp.search_try SET merchant_id = NULL;