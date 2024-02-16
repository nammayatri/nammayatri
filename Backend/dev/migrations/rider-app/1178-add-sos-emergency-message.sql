WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'SEND_SOS_ALERT', '{#userName#} has activated SOS/ emergency during their Namma Yatri ride. Ride Journey link here {#rideLink#}', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'MARK_RIDE_AS_SAFE', '{#userName#} has confirmed their safety during the Namma Yatri ride - Juspay.', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

ALTER TABLE atlas_app.person ADD COLUMN share_emergency_contacts boolean NOT NULL default false,
  ADD COLUMN trigger_support boolean NOT NULL default true,
  ADD COLUMN night_safety_checks boolean NOT NULL default true,
  ADD COLUMN has_completed_safety_setup boolean NOT NULL default false;

ALTER TABLE atlas_app.merchant ADD COLUMN tracking_short_url_pattern text DEFAULT 'nammayatri.in/t/' NOT NULL;

INSERT INTO atlas_app.rider_config (merchant_operating_city_id, merchant_id)
  SELECT T1.id, T1.merchant_id FROM atlas_app.merchant_operating_city as T1;

-- For Yatri Sathi (Kolkata) and Yatri (Kochi) in master and prod env
UPDATE atlas_app.merchant_message SET message = '{#userName#} has activated SOS/ emergency during YatriSathi ride. Ride Journey link here {#rideLink#}'
WHERE message_key = 'SEND_SOS_ALERT' AND merchant_operating_city_id = '';
UPDATE atlas_app.merchant_message SET message = '{#userName#} has confirmed their safety during the Yatri Sathi ride - Juspay'
WHERE message_key = 'MARK_RIDE_AS_SAFE' AND merchant_operating_city_id = '';
UPDATE atlas_app.merchant_message SET message = '{#userName#} has activated SOS/ emergency during Yatri ride. Ride Journey link here {#rideLink#}'
WHERE message_key = 'SEND_SOS_ALERT' AND merchant_operating_city_id = '';
UPDATE atlas_app.merchant_message SET message = '{#userName#} has confirmed their safety during the Yatri ride - Juspay'
WHERE message_key = 'MARK_RIDE_AS_SAFE' AND merchant_operating_city_id = '';

-- For NammaYatri (Bangalore) in master and prod env
UPDATE atlas_app.rider_config SET local_police_number = '', enable_local_police_support = true WHERE merchant_operating_city_id = '';

-- For Yatri Sathi only (Kolkata)
UPDATE atlas_app.rider_config SET enable_support_for_safety = true WHERE merchant_operating_city_id = '';