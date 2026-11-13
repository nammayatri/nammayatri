-- FRFS push sent when the bus driver ends a trip (Tools/Notifications.notifyBusTripEnded, key BUS_TRIP_ENDED).
-- Sent silently (DO_NOT_SHOW); without a row, dynamicNotifyPerson only logs "MISSED_FCM - BUS_TRIP_ENDED" and sends nothing.
--
-- Scope: ONLY merchant_operating_city_id = da7e19c8-2d86-66ea-039c-edfd8e34ebdc
-- Prod: run with 62f14d5f-8dca-bfa1-a73e-c3b14267e637 (JATRI_SAATHI / Kolkata) in place of the id above.
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
    'BUS_TRIP_ENDED',
    moc.merchant_id,
    moc.id,
    'Your trip has ended',
    'Your ride on route {#routeName#} has ended.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = 'da7e19c8-2d86-66ea-039c-edfd8e34ebdc'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'BUS_TRIP_ENDED'
      AND mpn.merchant_operating_city_id = moc.id
);
