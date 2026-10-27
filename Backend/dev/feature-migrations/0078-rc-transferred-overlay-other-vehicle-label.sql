-- RC transfer overlay variant for a previous holder who still has another verified RC linked
-- (SharedLogic.RCTransferNotice looks it up with udf1 = 'HAS_OTHER_VEHICLES'). Only the OK button label
-- differs: "Switch vehicle" instead of "Add vehicle". Everything else is copied from the base row
-- (udf1 NULL) of the same city and language, so run this after 0075 and 0077. 0076 updates every row of
-- the key, so running it before or after this is fine. A city without these rows falls back to the base
-- row, so the push still goes out with "Add vehicle".
--
-- Idempotent: guarded by NOT EXISTS on (merchant_operating_city_id, overlay_key, language, udf1).
--
-- Rollback:
--   DELETE FROM atlas_driver_offer_bpp.merchant_overlay
--   WHERE overlay_key = 'RC_TRANSFERRED_TO_ANOTHER_ACCOUNT' AND udf1 = 'HAS_OTHER_VEHICLES';

INSERT INTO atlas_driver_offer_bpp.merchant_overlay
  (id, language, merchant_id, merchant_operating_city_id, overlay_key, udf1, vehicle_category,
   title, description, ok_button_text, cancel_button_text,
   actions, actions2, secondary_actions, secondary_actions2, link, method, req_body, end_point,
   delay, contact_support_number, toast_message, show_push_notification, social_media_links, image_url)
SELECT
  md5(base.merchant_operating_city_id || '-' || base.overlay_key || '-HAS_OTHER_VEHICLES-' || base.language),
  base.language,
  base.merchant_id,
  base.merchant_operating_city_id,
  base.overlay_key,
  'HAS_OTHER_VEHICLES',
  base.vehicle_category,
  base.title,
  base.description,
  t.ok_button_text,
  base.cancel_button_text,
  base.actions,
  base.actions2,
  base.secondary_actions,
  base.secondary_actions2,
  base.link,
  base.method,
  base.req_body,
  base.end_point,
  base.delay,
  base.contact_support_number,
  base.toast_message,
  base.show_push_notification,
  base.social_media_links,
  base.image_url
FROM atlas_driver_offer_bpp.merchant_overlay base
JOIN (VALUES
  ('ENGLISH', 'Switch vehicle'),
  ('HINDI', 'वाहन बदलें'),
  ('KANNADA', 'ವಾಹನ ಬದಲಿಸಿ'),
  ('TAMIL', 'வாகனத்தை மாற்று'),
  ('TELUGU', 'వాహనాన్ని మార్చండి'),
  ('MALAYALAM', 'വാഹനം മാറ്റുക'),
  ('BENGALI', 'গাড়ি পরিবর্তন করুন'),
  ('ODIA', 'ଗାଡ଼ି ବଦଳାନ୍ତୁ'),
  ('FRENCH', 'Changer de véhicule')
) AS t(language, ok_button_text) ON t.language = base.language
WHERE base.overlay_key = 'RC_TRANSFERRED_TO_ANOTHER_ACCOUNT'
  AND base.udf1 IS NULL
  AND base.vehicle_category IS NULL
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.merchant_overlay mo
    WHERE mo.merchant_operating_city_id = base.merchant_operating_city_id
      AND mo.overlay_key = base.overlay_key
      AND mo.language = base.language
      AND mo.udf1 = 'HAS_OTHER_VEHICLES'
  );
