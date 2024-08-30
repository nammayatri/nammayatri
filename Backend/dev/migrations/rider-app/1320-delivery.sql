-- Creating idx on parties tables
CREATE INDEX idx_search_request_id ON atlas_app.search_request_parties_link USING btree (search_request_id);

CREATE INDEX idx_booking_id ON atlas_app.booking_parties_link USING btree (booking_id);
CREATE INDEX idx_party_id_and_is_active ON atlas_app.booking_parties_link USING btree (party_id, is_active);

--- Adding new sms for delivery --
WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'SMS_DELIVERY_DETAILS_SENDER',
'Dear user,
Please share the start OTP {#otp#} with the driver to confirm your parcel and start delivery.
Click on this link to track your delivery: {#trackingUrl#}
For any other details regarding the delivery install the app: {#appUrl#}
-Yatri Sathi' -- change merchant name as per requirement
  , T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);


WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'SMS_DELIVERY_DETAILS_RECEIVER',
'Dear user,
Your delivery is on its way. Click on this link to track your delivery: {#trackingUrl#}
Please share the end OTP {#otp#} with the driver to confirm receipt of your parcel.
For any other details regarding the delivery install the app: {#appUrl#}
-Yatri Sathi'  -- change merchant name as per requirement
  , T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'POST_DELIVERY_SENDER',
'Dear user,
Parcel is received by the receiver.
-Yatri Sathi' -- change merchant name as per requirement
  , T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);
