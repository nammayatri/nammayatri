CREATE INDEX idx_hv_verification_req_id ON atlas_driver_offer_bpp.hyperverge_verification USING btree (request_id);

CREATE INDEX idx_hv_verification_driver_id ON atlas_driver_offer_bpp.hyperverge_verification USING btree (driver_id);

INSERT INTO atlas_driver_offer_bpp.merchant_service_config
(
  merchant_id,
  merchant_operating_city_id,
  service_name,
  config_json
)
SELECT
  merchant_id,
  id,
  'Verification_HyperVergeRCDL',
  '{"username":"dummy","password":"0.1.0|8|xgWOO3UTpKVvav8nc93+KVrEneQHUXMlXy1vpcsaoC/RKjHC3YTGcePB00hiZSsgENimo1Gy938=","url":"http://dummy","appId":"dummy","appKey":"0.1.0|8|xgWOO3UTpKVvav8nc93+KVrEneQHUXMlXy1vpcsaoC/RKjHC3YTGcePB00hiZSsgENimo1Gy938="}'  --This password is master passetto encrypted. Please re-encrypt before running in prod.
FROM atlas_driver_offer_bpp.merchant_operating_city;