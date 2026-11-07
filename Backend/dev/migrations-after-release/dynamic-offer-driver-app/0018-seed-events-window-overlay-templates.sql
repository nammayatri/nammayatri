-- Default-city templates for the two overlays that EVENTS versions ("offences in the
-- driver's last X events") of the cancellation-rate behaviour use:
-- CANCELLATION_RATE_NUDGE_EVENTS and CANCELLATION_RATE_WARN_EVENTS. Behaviour Enable copies a
-- version's overlays from the "default" city into the city it enables, so without these
-- templates EVENTS nudges and warns fire but show the driver nothing.
--
-- For now both use the same content as CANCELLATION_RATE_NUDGE_DAILY: one row per
-- language / udf1 / vehicle category, taken from a single city that has it. Replace the
-- text once product has EVENTS-specific copy.
-- Idempotent: rows that already exist under "default" are left alone.
INSERT INTO atlas_driver_offer_bpp.merchant_overlay
  (id, merchant_id, merchant_operating_city_id, overlay_key, language, udf1, vehicle_category,
   title, description, image_url, ok_button_text, cancel_button_text, actions, actions2,
   secondary_actions, secondary_actions2, link, req_body, end_point, method, delay,
   contact_support_number, toast_message, social_media_links, show_push_notification)
SELECT gen_random_uuid()::text, src.merchant_id, 'default', keys.overlay_key, src.language, src.udf1, src.vehicle_category,
       src.title, src.description, src.image_url, src.ok_button_text, src.cancel_button_text, src.actions, src.actions2,
       src.secondary_actions, src.secondary_actions2, src.link, src.req_body, src.end_point, src.method, src.delay,
       src.contact_support_number, src.toast_message, src.social_media_links, src.show_push_notification
FROM (
    SELECT DISTINCT ON (language, udf1, vehicle_category) *
    FROM atlas_driver_offer_bpp.merchant_overlay
    WHERE overlay_key = 'CANCELLATION_RATE_NUDGE_DAILY'
      AND merchant_operating_city_id <> 'default'
    ORDER BY language, udf1, vehicle_category, merchant_operating_city_id
) AS src
CROSS JOIN (VALUES ('CANCELLATION_RATE_NUDGE_EVENTS'), ('CANCELLATION_RATE_WARN_EVENTS')) AS keys(overlay_key)
WHERE NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.merchant_overlay existing
    WHERE existing.merchant_operating_city_id = 'default'
      AND existing.overlay_key = keys.overlay_key
      AND existing.language = src.language
      AND existing.udf1 IS NOT DISTINCT FROM src.udf1
      AND existing.vehicle_category IS NOT DISTINCT FROM src.vehicle_category
);
