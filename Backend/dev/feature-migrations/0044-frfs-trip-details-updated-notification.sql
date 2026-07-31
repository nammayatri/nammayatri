-- FRFS bus "trip details updated" push notification.
--
-- Fired by the operator waybill-details update (waybill/details/v2 -> fanOutWaybillRefresh) when the
-- driver and/or the assigned bus actually changes on a confirmed booking. Push-only. NOT tier-gated
-- (unlike BUS_TRIP_STARTED): every affected confirmed rider on the waybill is notified.
--
-- The `{#updatedField#}` variable is filled in code with one of:
--   "driver details" / "bus number" / "driver and bus details".
-- `{#routeName#}` and `{#vehicleNumber#}` are also available.
--
-- Scope: ONLY merchant_operating_city_id = da7e19c8-2d86-66ea-039c-edfd8e34ebdc
-- (same city as 0039-frfs-bus-trip-notifications). Add cities as the feature rolls out.
--
-- Idempotent: safe to re-run.

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    title,
    body,
    language,
    should_trigger,
    created_at,
    updated_at
)
SELECT
    'TRIGGER_FCM',
    'FRFS_TRIP_DETAILS_UPDATED',
    moc.merchant_id,
    moc.id,
    'Bus trip details updated',
    'Update for your trip on route {#routeName#}: {#updatedField#} changed. Open the app to see the latest.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = 'da7e19c8-2d86-66ea-039c-edfd8e34ebdc'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'FRFS_TRIP_DETAILS_UPDATED'
      AND mpn.merchant_operating_city_id = moc.id
);

------------------------------------------------------------------------------------------------------
-- WhatsApp: trip details updated (secondary channel, opt-in riders only).
-- Positional template variables (order must match the code in notifyFrfsTripDetailsUpdated):
--   {{1}} = updatedField ("driver details" / "bus number" / "driver and bus details")
--   {{2}} = vehicleNumber
--   {{3}} = routeName ("<fromStop> - <toStop>")
-- The template carries a STATIC URL button (the app link) baked into the template definition, so
-- contains_url_button = true and NO url variable is passed (mirrors WHATSAPP_BUS_TRIP_STARTED).
-- template_id 8769356 = the approved Gupshup "bus_trip_details_updated" Utility template
-- (variables: {{1}}=updatedField, {{2}}=vehicleNumber, {{3}}=routeName).
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.merchant_message (
    merchant_id,
    merchant_operating_city_id,
    message_key,
    message,
    template_id,
    contains_url_button,
    created_at,
    updated_at
)
SELECT
    moc.merchant_id,
    moc.id,
    'WHATSAPP_FRFS_TRIP_DETAILS_UPDATED',
    E'Hello, there has been an update to the {#updatedField#} for your upcoming bus trip.\n\n🚌 Bus: {#vehicleNumber#}\n🛣️ Route: {#routeName#}\n\nPlease open the Yatri Sathi app to view the latest details.',
    '8769356',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = 'da7e19c8-2d86-66ea-039c-edfd8e34ebdc'
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;
