-- Overlay for a driver whose RC was activated by another driver (SharedLogic.RCTransferNotice).
-- Sent once as a DRIVER_NOTIFY push (driver_notification_payload); nothing is stored or re-shown.
-- {#rcNumber#} is filled with the full RC number (the previous holder's own vehicle; logs stay masked).
-- req_body {"overlayKey": ...} only tells the driver app to use the RC overlay layout. end_point and method
-- stay NULL and CALL_API is never in actions, so the body is never sent anywhere.
-- actions: OPEN_APP brings the app up. 0076 switches confirmed brands to OPEN_LINK (Vehicle tab deep
-- link); it is gated on the driver APK release.
-- Support: contact_support_number is left NULL; the backend sends merchant_operating_city.support_number
-- at runtime (a number set on the overlay row wins), so a city's support number stays current. The app
-- adds its own "Need help" CALL_SUPPORT button when a number is present, so secondary_actions stays empty
-- (listing CALL_SUPPORT too opens the dialer twice). cancel_button_text labels that button; the backend
-- sends it only when there is a number. Cities without a support_number show no button.
-- Seeds ENGLISH for every city. Other languages are seeded in 0077; a language without a row falls back
-- to the ENGLISH row.
-- Idempotent (NOT EXISTS guard).
INSERT INTO atlas_driver_offer_bpp.merchant_overlay
  (id, language, merchant_id, merchant_operating_city_id, overlay_key, title, description, ok_button_text,
   cancel_button_text, actions, secondary_actions, req_body, show_push_notification)
SELECT
  md5(moc.id || '-RC_TRANSFERRED_TO_ANOTHER_ACCOUNT-ENGLISH'),
  'ENGLISH',
  moc.merchant_id,
  moc.id,
  'RC_TRANSFERRED_TO_ANOTHER_ACCOUNT',
  'RC linked to another account',
  'Vehicle {#rcNumber#} is now linked to another account.',
  'Add vehicle',
  'Call Support',
  ARRAY['OPEN_APP'],
  ARRAY[]::text[],
  '{"overlayKey": "RC_TRANSFERRED_TO_ANOTHER_ACCOUNT"}'::json,
  true
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.merchant_overlay mo
  WHERE mo.merchant_operating_city_id = moc.id
    AND mo.overlay_key = 'RC_TRANSFERRED_TO_ANOTHER_ACCOUNT'
    AND mo.language = 'ENGLISH'
);
