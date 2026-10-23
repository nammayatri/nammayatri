-- Overlays for a driver whose RC was activated by another driver (SharedLogic.RCTransferNotice).
-- Pushed live via DRIVER_NOTIFY (driver_notification_payload) and returned on GET /ui/driver/profile
-- as pendingRcTransferNotice until acknowledged or the driver activates a vehicle.
--   RC_TRANSFERRED_VEHICLE_REMOVED: driver still has another linked valid RC.
--   RC_TRANSFERRED_NO_VEHICLE:      driver has no other vehicle to switch to.
-- {#rcNumber#} is filled with the masked RC number. The backend sets end_point (absolute ack URL built
-- from selfUIUrl), method and req_body ({"transferId": ...}), so they are not configured here.
-- actions: CALL_API acks the notice, OPEN_APP then brings the app up. 0076 switches confirmed brands
-- to OPEN_LINK (Vehicle tab deep link); it is gated on the driver APK release.
-- Support: set contact_support_number per city; the app adds its own "Need help" CALL_SUPPORT button
-- when it is present, so secondary_actions stays empty (listing CALL_SUPPORT too opens the dialer twice).
-- Seeds ENGLISH for every city; add rows per language as needed (no row => no overlay, warning logged).
-- Idempotent (NOT EXISTS guard).
INSERT INTO atlas_driver_offer_bpp.merchant_overlay
  (id, language, merchant_id, merchant_operating_city_id, overlay_key, title, description, ok_button_text,
   actions, secondary_actions, show_push_notification)
SELECT
  md5(moc.id || '-' || v.overlay_key || '-ENGLISH'),
  'ENGLISH',
  moc.merchant_id,
  moc.id,
  v.overlay_key,
  v.title,
  v.description,
  v.ok_button_text,
  ARRAY['CALL_API', 'OPEN_APP'],
  ARRAY[]::text[],
  true
FROM atlas_driver_offer_bpp.merchant_operating_city moc
CROSS JOIN (VALUES
  ('RC_TRANSFERRED_VEHICLE_REMOVED',
   'Vehicle removed from your account',
   'Vehicle {#rcNumber#} is now linked to another account and has been removed from yours. Switch to another vehicle to continue taking rides.',
   'Okay'),
  ('RC_TRANSFERRED_NO_VEHICLE',
   'You can''t take rides right now',
   'Vehicle {#rcNumber#} is now linked to another account. You can''t take rides until you add a vehicle. Upload your vehicle documents again or contact support if this is a mistake.',
   'Add vehicle')
) AS v(overlay_key, title, description, ok_button_text)
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.merchant_overlay mo
  WHERE mo.merchant_operating_city_id = moc.id
    AND mo.overlay_key = v.overlay_key
    AND mo.language = 'ENGLISH'
);
