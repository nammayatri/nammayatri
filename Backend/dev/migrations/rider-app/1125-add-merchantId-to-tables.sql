ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN merchant_id character(36) REFERENCES atlas_app.merchant (id);

UPDATE atlas_app.booking_cancellation_reason SET merchant_id = NULL;
ALTER TABLE atlas_app.driver_offer ADD COLUMN merchant_id character(36) REFERENCES atlas_app.merchant (id);

UPDATE atlas_app.driver_offer SET merchant_id = NULL;


ALTER TABLE atlas_app.estimate ADD COLUMN merchant_id character(36) REFERENCES atlas_app.merchant (id);

UPDATE atlas_app.estimate SET merchant_id =  NULL;

ALTER TABLE atlas_app.ride ADD COLUMN merchant_id character(36) REFERENCES atlas_app.merchant (id);

UPDATE atlas_app.ride SET merchant_id = NULL;