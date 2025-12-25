WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'POST_RIDE_SOS', '{#userName#} has activated SOS/ emergency during their Namma Yatri ride. Ride ended at {#rideEndTime#} -Namma Yatri', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

-- For Yatri and Yatri Sathi, ManaYatri
-- UPDATE atlas_app.merchant_message SET message = '{#userName#} has activated SOS/ emergency during their Yatri Sathi ride. Ride ended at {#rideEndTime#} -Namma Yatri'
-- WHERE message_key = 'POST_RIDE_SOS' AND merchant_operating_city_id = '';
-- UPDATE atlas_app.merchant_message SET message = '{#userName#} has activated SOS/ emergency during their Yatri ride. Ride ended at {#rideEndTime#} -Namma Yatri'
-- WHERE message_key = 'POST_RIDE_SOS' AND merchant_operating_city_id = '';
-- UPDATE atlas_app.merchant_message SET message = '{#userName#} has activated SOS/ emergency during their Mana Yatri ride. Ride ended at {#rideEndTime#} -Mana Yatri'


UPDATE atlas_app.rider_config SET tracking_short_url_pattern = 'https://nammayatri.in/u?vp=shareRide&rideId=';
