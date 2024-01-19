ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN contact_person_id text;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN enable_for_following boolean NOT NULL DEFAULT FALSE;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN priority int NOT NULL DEFAULT 0;
ALTER TABLE atlas_app.person ADD COLUMN follows_ride boolean NOT NULL DEFAULT FALSE;


WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'FOLLOW_RIDE', '{#userName#} wants you to follow their Namma Yatri ride. Ride Journey link here {#rideLink#}', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);


-- -- For Yatri Sathi (Kolkata) and Yatri (Kochi) in master and prod env
-- UPDATE atlas_app.merchant_message SET message = '{#userName#} wants you to follow their Yatri Sathi ride. Ride Journey link here {#rideLink#}'
-- WHERE message_key = 'FOLLOW_RIDE' AND merchant_operating_city_id = '';
-- UPDATE atlas_app.merchant_message SET message = '{#userName#} wants you to follow their Yatri ride. Ride Journey link here {#rideLink#}'
-- WHERE message_key = 'FOLLOW_RIDE' AND merchant_operating_city_id = '';