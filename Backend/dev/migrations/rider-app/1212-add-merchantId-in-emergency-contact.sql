ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN merchant_id text;

ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN enable_for_share_ride boolean NOT NULL default false;

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'ADDED_AS_EMERGENCY_CONTACT', '{#userName#} has chosen you as their trusted emergency contact for their Namma Yatri rides. Ensure seamless tracking and safety by installing the app today {#appUrl#} - Juspay', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

UPDATE atlas_app.merchant_message SET message = '{#userName#} has chosen you as their trusted emergency contact for their Yatri Sathi rides. Ensure seamless tracking and safety by installing the app today {#appUrl#} - Juspay'
WHERE message_key = 'ADDED_AS_EMERGENCY_CONTACT' AND merchant_operating_city_id = '';

UPDATE atlas_app.merchant_message SET message = '{#userName#} has chosen you as their emergency contact for their Yatri rides. Ensure seamless tracking and safety by installing the app today {#appUrl#}'
WHERE message_key = 'ADDED_AS_EMERGENCY_CONTACT' AND merchant_operating_city_id = '';

ALTER TABLE atlas_app.merchant DROP COLUMN time_diff_from_utc ;

ALTER TABLE atlas_app.merchant DROP COLUMN tracking_short_url_pattern;