ALTER TABLE atlas_app.special_location ADD COLUMN merchant_id varchar(36);

ALTER TABLE atlas_app.gate_info ADD COLUMN merchant_operating_city_id varchar(36) ;
ALTER TABLE atlas_app.gate_info ADD COLUMN merchant_id varchar(36) ;

ALTER TABLE atlas_app.payment_transaction ADD COLUMN merchant_operating_city_id varchar(36) ;
