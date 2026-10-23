-- RC transfer overlays (RC_TRANSFERRED_NO_VEHICLE, RC_TRANSFERRED_VEHICLE_REMOVED, seeded in 0075):
-- the "Add vehicle" button opens the driver app's Vehicle tab instead of only opening the app.
-- CALL_API stays first so the notice is acked before the link opens.
--
-- ROLLOUT: OPEN_LINK opens inside the app only on driver builds with the MessagingView.openLink fix;
-- older builds open the link in the browser. Run this in prod only after that APK is out.
-- Rows are served from the overlay cache, so clear CachedQueries:Overlay:* (or wait for expiry) after running.
-- Rollback: set actions back to ARRAY['CALL_API', 'OPEN_APP'] and link to NULL for these keys.
--
-- Only merchants whose /p deep-link host is confirmed are switched; every other merchant stays on OPEN_APP.
-- Idempotent.
UPDATE atlas_driver_offer_bpp.merchant_overlay mo
SET actions = ARRAY['CALL_API', 'OPEN_LINK'],
    link = 'https://' || h.host || '/p?vp=profile&tab=vehicle'
FROM atlas_driver_offer_bpp.merchant_operating_city moc
JOIN (VALUES
  ('NAMMA_YATRI_PARTNER', 'nammayatri.in'),
  ('JATRI_SAATHI_PARTNER', 'yatrisathi.in'),
  ('BRIDGE_CABS_PARTNER', 'bridge.cab'),
  ('BHARAT_TAXI_PARTNER', 'www.bharattaxiapp.com')
) AS h(merchant_short_id, host) ON h.merchant_short_id = moc.merchant_short_id
WHERE mo.merchant_operating_city_id = moc.id
  AND mo.overlay_key IN ('RC_TRANSFERRED_NO_VEHICLE', 'RC_TRANSFERRED_VEHICLE_REMOVED');
