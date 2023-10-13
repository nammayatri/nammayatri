ALTER TABLE atlas_driver_offer_bpp.merchant_message ADD COLUMN template_id character varying(255);
ALTER TABLE atlas_driver_offer_bpp.merchant_message ADD COLUMN json_data json;
ALTER TABLE atlas_driver_offer_bpp.merchant_message ADD COLUMN contains_url_button boolean DEFAULT false;

UPDATE atlas_driver_offer_bpp.merchant_message SET contains_url_button = false;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN volunteer_sms_sending_limit json;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_sms_receiving_limit json;

UPDATE atlas_driver_offer_bpp.transporter_config set volunteer_sms_sending_limit = json_build_object ('sms', 500, 'whatsapp', 500, 'overlay', 500, 'alert', 500) ;
UPDATE atlas_driver_offer_bpp.transporter_config set driver_sms_receiving_limit = json_build_object ('sms', 5, 'whatsapp', 5, 'overlay', 5, 'alert', 5) ;

ALTER TABLE atlas_driver_offer_bpp.merchant_message ALTER COLUMN message TYPE text ;

-- whatsapp messages

WITH MerchantMessages AS (
  SELECT T1.id, 'WHATSAPP_CLEAR_DUES_CALL_MISSED_MESSAGE', '6830100', CAST ('{"var1" : "https://nammayatri.in/p/?vp=plans", "var2" : "https://www.youtube.com/shorts/x9cJN78j9V8"}' AS json), 'Dear Namma Yatri Partner,

ನಾವು ನಿಮಗೆ ಕರೆ ಮಾಡಲು ಪ್ರಯತ್ನಿಸಿದ್ದೇವೆ ಆದರೆ ಸಂಪರ್ಕಿಸಲು ಸಾಧ್ಯವಾಗಲಿಲ್ಲ.
ತಡೆರಹಿತ ಸವಾರಿಗಳನ್ನು ಮುಂದುವರಿಸಲು ನಮ್ಮ ಯಾತ್ರಿ ಅಪ್ಲಿಕೇಶನ್‌ನಲ್ಲಿ ನಿಮ್ಮ ಸಬ್‌ಸ್ಕ್ರಿಪ್‌ಶನ್ ಪ್ಲಾನಿನ ಬಾಕಿಗಳನ್ನು ದಯವಿಟ್ಟು ಕ್ಲಿಯರ್ ಮಾಡಿ.
ನಮ್ಮ ಯಾತ್ರಿ ! ಪ್ರತಿ ಹೆಜ್ಜೆಗು ನಿಮ್ಮ ಮಾರ್ಗದರ್ಶಿ!

We tried calling you but couldn''t connect.
Kindly clear your subscription plan dues now on Namma Yatri app to continue taking non-stop rides.

ಕ್ಲಿಕ್ / Click - {#var1#}

ವೀಡಿಯೊ ವೀಕ್ಷಿಸಲು / Watch video - {#var2#}', true
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);


WITH MerchantMessages AS (
  SELECT T1.id, 'WHATSAPP_CLEAR_DUES_MESSAGE', '6830099', CAST ('{"var1" : "https://nammayatri.in/p/?vp=plans", "var2" : "https://www.youtube.com/shorts/x9cJN78j9V8"}' AS json), 'Dear Namma Yatri Partner,

ತಡೆರಹಿತ ಸವಾರಿಗಳನ್ನು ಮುಂದುವರಿಸಲು ನಮ್ಮ ಯಾತ್ರಿ ಅಪ್ಲಿಕೇಶನ್‌ನಲ್ಲಿ ನಿಮ್ಮ ಸಬ್‌ಸ್ಕ್ರಿಪ್‌ಶನ್ ಪ್ಲಾನಿನ ಬಾಕಿಗಳನ್ನು ದಯವಿಟ್ಟು ಕ್ಲಿಯರ್ ಮಾಡಿ.
ನಮ್ಮ ಯಾತ್ರಿ ! ಪ್ರತಿ ಹೆಜ್ಜೆಗು ನಿಮ್ಮ ಮಾರ್ಗದರ್ಶಿ!

Kindly clear your subscription plan dues now on Namma Yatri app to continue taking non-stop rides.

ಕ್ಲಿಕ್ / Click - {#var1#}

ವೀಡಿಯೊ ವೀಕ್ಷಿಸಲು / Watch video - {#var2#}', true
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);


WITH MerchantMessages AS (
  SELECT T1.id, 'WHATSAPP_CLEAR_DUES_MESSAGE_TO_BLOCKED_DRIVERS', '6830098', CAST ('{"var1" : "https://nammayatri.in/p/?vp=plans", "var2" : "https://www.youtube.com/shorts/x9cJN78j9V8"}' AS json), 'Dear Namma Yatri Partner,

ನಿಮ್ಮ ಸಬ್‌ಸ್ಕ್ರಿಪ್‌ಶನ್ ಪ್ಲಾನ್‌ನಲ್ಲಿ ಬಾಕಿಗಳಿರುವ ಕಾರಣ ನಮ್ಮ ಯಾತ್ರಿಯಲ್ಲಿ ಸವಾರಿಗಳನ್ನು ಸ್ವೀಕರಿಸುವುದನ್ನು ನಿಲ್ಲಿಸಿದ್ದೀರಿ.
ತಡೆರಹಿತ ಸವಾರಿಗಳನ್ನು ಮುಂದುವರಿಸಲು ದಯವಿಟ್ಟು ನಿಮ್ಮ ಬಾಕಿಯನ್ನು ಈಗ ನಮ್ಮ ಯಾತ್ರಿ ಅಪ್ಲಿಕೇಶನ್‌ನಲ್ಲಿ ಪಾವತಿಸಿ.

You have stopped receiving rides on Namma Yatri as you have pending subscription dues.
Kindly pay your dues now on the app to continue taking non-stop rides.

ಕ್ಲಿಕ್ / Click - {#var1#}

ವೀಡಿಯೊ ವೀಕ್ಷಿಸಲು / Watch video - {#var2#}', true
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);


WITH MerchantMessages AS (
  SELECT T1.id, 'WHATSAPP_SETUP_AUTOPAY_MESSAGE', '6830093', CAST ('{"var1" : "https://nammayatri.in/p/?vp=plans", "var2" : "https://www.youtube.com/watch?v=3I3adSdYeX8"}' AS json), 'Dear Namma Yatri Partner,

ಆಟೋಪೇಯು ಸಬ್‌ಸ್ಕ್ರಿಪ್‌ಶನ್ ಬಾಕಿ ಪಾವತಿಗಳನ್ನು ಮಾಡಲು ಅನುಕೂಲಕರ ವಿಧಾನವಾಗಿದೆ.
ತಡೆರಹಿತ ಸವಾರಿಗಳನ್ನು ಮುಂದುವರಿಸಲು ಇದೀಗ ಆಟೋಪೇಯನ್ನು ಹೊಂದಿಸಿ.

Autopay is a convenient method to make subscription due payments. Set-up Autopay now.

ಕ್ಲಿಕ್ / Click - {#var1#}

ವೀಡಿಯೊ ವೀಕ್ಷಿಸಲು / Watch video - {#var2#}', true
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.id, 'WHATSAPP_SWITCH_PLAN_MESSAGE', '6830027', CAST ('{"var1" : "https://nammayatri.in/p/?vp=plans", "var2" : "https://www.youtube.com/watch?v=Ygq5j801YkA"}' AS json), 'Dear Namma Yatri Partner,

ನಮ್ಮ ಯಾತ್ರಿ ಅಪ್ಲಿಕೇಶನ್‌ನಲ್ಲಿ ಪ್ಲಾನ್ ಅನ್ನು ಬದಲಾಯಿಸುವ ಮೂಲಕ ನೀವು ಈಗ ಹೆಚ್ಚಿನದನ್ನು ಉಳಿಸಬಹುದು.

You can now save more by switching plans on the Namma Yatri App.

ಕ್ಲಿಕ್ / Click - {#var1#}

ವೀಡಿಯೊ ವೀಕ್ಷಿಸಲು / Watch video - {#var2#}', true
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);


WITH MerchantMessages AS (
  SELECT T1.id, 'WHATSAPP_HOW_IT_WORKS_MESSAGE', '6830156', CAST ('{"var1" : "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK"}' AS json), 'Dear Namma Yatri Partner,

ನಮ್ಮ ಯಾತ್ರಿ ಸಬ್‌ಸ್ಕ್ರಿಪ್‌ಶನ್ ಪ್ಲಾನ್ ಅನ್ನು ಆಯ್ಕೆ ಮಾಡಲು ಮತ್ತು UPI ಆಟೋಪೇಯ ಕುರಿತು ಇನ್ನಷ್ಟು ತಿಳಿದುಕೊಳ್ಳಲು, ವೀಡಿಯೊವನ್ನು ವೀಕ್ಷಿಸಿ.

To select a Namma Yatri Subscription plan and learn more about UPI Autopay, watch the video.

ಕ್ಲಿಕ್ / Click - {#var#}', true
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);


-- sms messages


WITH MerchantMessages AS (
  SELECT T1.id, 'SMS_CLEAR_DUES_CALL_MISSED_MESSAGE', null, CAST ('{"var1" : "https://nammayatri.in/p/?vp=plans", "var2" : "https://www.youtube.com/shorts/x9cJN78j9V8"}' AS json), 'Dear Namma Yatri Partner,

ನಾವು ನಿಮಗೆ ಕರೆ ಮಾಡಲು ಪ್ರಯತ್ನಿಸಿದ್ದೇವೆ ಆದರೆ ಸಂಪರ್ಕಿಸಲು ಸಾಧ್ಯವಾಗಲಿಲ್ಲ.
ತಡೆರಹಿತ ಸವಾರಿಗಳನ್ನು ಮುಂದುವರಿಸಲು ನಮ್ಮ ಯಾತ್ರಿ ಅಪ್ಲಿಕೇಶನ್‌ನಲ್ಲಿ ನಿಮ್ಮ ಸಬ್‌ಸ್ಕ್ರಿಪ್‌ಶನ್ ಪ್ಲಾನಿನ ಬಾಕಿಗಳನ್ನು ದಯವಿಟ್ಟು ಕ್ಲಿಯರ್ ಮಾಡಿ.
ನಮ್ಮ ಯಾತ್ರಿ ! ಪ್ರತಿ ಹೆಜ್ಜೆಗು ನಿಮ್ಮ ಮಾರ್ಗದರ್ಶಿ!

We tried calling you but couldn''t connect.
Kindly clear your subscription plan dues now on Namma Yatri app to continue taking non-stop rides.

ಕ್ಲಿಕ್ / Click - {#var1#}

ವೀಡಿಯೊ ವೀಕ್ಷಿಸಲು / Watch video - {#var2#}', false
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);


WITH MerchantMessages AS (
  SELECT T1.id, 'SMS_CLEAR_DUES_MESSAGE', null, CAST ('{"var1" : "https://nammayatri.in/p/?vp=plans", "var2" : "https://www.youtube.com/shorts/x9cJN78j9V8"}' AS json), 'Dear Namma Yatri Partner,

ತಡೆರಹಿತ ಸವಾರಿಗಳನ್ನು ಮುಂದುವರಿಸಲು ನಮ್ಮ ಯಾತ್ರಿ ಅಪ್ಲಿಕೇಶನ್‌ನಲ್ಲಿ ನಿಮ್ಮ ಸಬ್‌ಸ್ಕ್ರಿಪ್‌ಶನ್ ಪ್ಲಾನಿನ ಬಾಕಿಗಳನ್ನು ದಯವಿಟ್ಟು ಕ್ಲಿಯರ್ ಮಾಡಿ.
ನಮ್ಮ ಯಾತ್ರಿ ! ಪ್ರತಿ ಹೆಜ್ಜೆಗು ನಿಮ್ಮ ಮಾರ್ಗದರ್ಶಿ!

Kindly clear your subscription plan dues now on Namma Yatri app to continue taking non-stop rides.

ಕ್ಲಿಕ್ / Click - {#var1#}

ವೀಡಿಯೊ ವೀಕ್ಷಿಸಲು / Watch video - {#var2#}', false
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);


WITH MerchantMessages AS (
  SELECT T1.id, 'SMS_CLEAR_DUES_MESSAGE_TO_BLOCKED_DRIVERS', null, CAST ('{"var1" : "https://nammayatri.in/p/?vp=plans", "var2" : "https://www.youtube.com/shorts/x9cJN78j9V8"}' AS json), 'Dear Namma Yatri Partner,

ನಿಮ್ಮ ಸಬ್‌ಸ್ಕ್ರಿಪ್‌ಶನ್ ಪ್ಲಾನ್‌ನಲ್ಲಿ ಬಾಕಿಗಳಿರುವ ಕಾರಣ ನಮ್ಮ ಯಾತ್ರಿಯಲ್ಲಿ ಸವಾರಿಗಳನ್ನು ಸ್ವೀಕರಿಸುವುದನ್ನು ನಿಲ್ಲಿಸಿದ್ದೀರಿ.
ತಡೆರಹಿತ ಸವಾರಿಗಳನ್ನು ಮುಂದುವರಿಸಲು ದಯವಿಟ್ಟು ನಿಮ್ಮ ಬಾಕಿಯನ್ನು ಈಗ ನಮ್ಮ ಯಾತ್ರಿ ಅಪ್ಲಿಕೇಶನ್‌ನಲ್ಲಿ ಪಾವತಿಸಿ.

You have stopped receiving rides on Namma Yatri as you have pending subscription dues.
Kindly pay your dues now on the app to continue taking non-stop rides.

ಕ್ಲಿಕ್ / Click - {#var1#}

ವೀಡಿಯೊ ವೀಕ್ಷಿಸಲು / Watch video - {#var2#}', false
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);


WITH MerchantMessages AS (
  SELECT T1.id, 'SMS_SETUP_AUTOPAY_MESSAGE', null, CAST ('{"var1" : "https://nammayatri.in/p/?vp=plans", "var2" : "https://www.youtube.com/watch?v=3I3adSdYeX8"}' AS json), 'Dear Namma Yatri Partner,

ಆಟೋಪೇಯು ಸಬ್‌ಸ್ಕ್ರಿಪ್‌ಶನ್ ಬಾಕಿ ಪಾವತಿಗಳನ್ನು ಮಾಡಲು ಅನುಕೂಲಕರ ವಿಧಾನವಾಗಿದೆ.
ತಡೆರಹಿತ ಸವಾರಿಗಳನ್ನು ಮುಂದುವರಿಸಲು ಇದೀಗ ಆಟೋಪೇಯನ್ನು ಹೊಂದಿಸಿ.

Autopay is a convenient method to make subscription due payments. Set-up Autopay now.

ಕ್ಲಿಕ್ / Click - {#var1#}

ವೀಡಿಯೊ ವೀಕ್ಷಿಸಲು / Watch video - {#var2#}', false
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);


WITH MerchantMessages AS (
  SELECT T1.id, 'SMS_SWITCH_PLAN_MESSAGE', null, CAST ('{"var1" : "https://nammayatri.in/p/?vp=plans", "var2" : "https://www.youtube.com/watch?v=Ygq5j801YkA"}' AS json), 'Dear Namma Yatri Partner,

ನಮ್ಮ ಯಾತ್ರಿ ಅಪ್ಲಿಕೇಶನ್‌ನಲ್ಲಿ ಪ್ಲಾನ್ ಅನ್ನು ಬದಲಾಯಿಸುವ ಮೂಲಕ ನೀವು ಈಗ ಹೆಚ್ಚಿನದನ್ನು ಉಳಿಸಬಹುದು.

You can now save more by switching plans on the Namma Yatri App.

ಕ್ಲಿಕ್ / Click - {#var1#}

ವೀಡಿಯೊ ವೀಕ್ಷಿಸಲು / Watch video - {#var2#}', false
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);


WITH MerchantMessages AS (
  SELECT T1.id, 'SMS_HOW_IT_WORKS_MESSAGE', null, CAST ('{"var1" : "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK"}' AS json), 'Dear Namma Yatri Partner,

ನಮ್ಮ ಯಾತ್ರಿ ಸಬ್‌ಸ್ಕ್ರಿಪ್‌ಶನ್ ಪ್ಲಾನ್ ಅನ್ನು ಆಯ್ಕೆ ಮಾಡಲು ಮತ್ತು UPI ಆಟೋಪೇಯ ಕುರಿತು ಇನ್ನಷ್ಟು ತಿಳಿದುಕೊಳ್ಳಲು, ವೀಡಿಯೊವನ್ನು ವೀಕ್ಷಿಸಿ.

To select a Namma Yatri Subscription plan and learn more about UPI Autopay, watch the video.

ಕ್ಲಿಕ್ / Click - {#var1#}', false
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, template_id, json_data, message, contains_url_button)
  (SELECT * FROM MerchantMessages);