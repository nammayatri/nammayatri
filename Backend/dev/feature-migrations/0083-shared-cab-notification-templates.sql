-- Shared-cab push-notification templates, rider and driver. ENGLISH only. Idempotent: inserts skip rows that exist, and the
-- updates bring rows seeded with earlier copy to the current text.

-- ── Rider pushes (atlas_app), keys = SharedLogic.SharedCab.Notify.SharedCabNotificationType ──
-- Params: {#boardStop#}, {#dropStop#}, {#vehicleNumber#} (only when a cab is known), {#vehicleLast4#} and {#boardDeadlineSec#}
-- (SHARED_CAB_ARRIVING), {#missedCabs#} (SHARED_CAB_BOOKING_CANCELLED). Seeded only for cities with a shared-cab feed.
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
    copy.key,
    moc.merchant_id,
    moc.id,
    copy.title,
    copy.body,
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
CROSS JOIN (VALUES
    ('SHARED_CAB_ASSIGNED', 'Your shared cab is on its way', 'Cab {#vehicleNumber#} will pick you up at {#boardStop#}.'),
    ('SHARED_CAB_ARRIVING', 'Your cab is here', 'Cab {#vehicleLast4#} is at {#boardStop#}. Board within {#boardDeadlineSec#}s and enter the code inside.'),
    ('SHARED_CAB_REASSIGNED', 'You missed your cab', 'We are finding you another cab to {#dropStop#}.'),
    ('SHARED_CAB_BOARD_ANY', 'Board any shared cab', 'Board any shared cab to {#dropStop#} at {#boardStop#} and enter the code inside it.'),
    ('SHARED_CAB_ROUTE_CHANGE', 'Your cab is changing route', 'Cab {#vehicleNumber#} will not go on to {#dropStop#}. Get down at the next common stop.'),
    ('SHARED_CAB_BOOKING_CANCELLED', 'Your booking was cancelled', 'Your booking was cancelled after {#missedCabs#} missed cabs.'),
    ('SHARED_CAB_FINDING_TIMEOUT', 'No cab found', 'We could not find a shared cab for your booking, so it was cancelled. Your fare will be refunded in full.')
) AS copy (key, title, body)
WHERE EXISTS (
    SELECT 1
    FROM atlas_app.integrated_bpp_config ibc
    WHERE ibc.merchant_operating_city_id = moc.id
      AND ibc.agency_key LIKE '%:SHARED_CAB'
)
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = copy.key
      AND mpn.merchant_operating_city_id = moc.id
);

-- Rows seeded before SHARED_CAB_ARRIVING carried the boarding countdown and SHARED_CAB_REASSIGNED read right for a missed cab.
UPDATE atlas_app.merchant_push_notification
SET title = 'Your cab is here',
    body = 'Cab {#vehicleLast4#} is at {#boardStop#}. Board within {#boardDeadlineSec#}s and enter the code inside.',
    updated_at = CURRENT_TIMESTAMP
WHERE key = 'SHARED_CAB_ARRIVING'
  AND language = 'ENGLISH'
  AND body <> 'Cab {#vehicleLast4#} is at {#boardStop#}. Board within {#boardDeadlineSec#}s and enter the code inside.';

UPDATE atlas_app.merchant_push_notification
SET title = 'You missed your cab',
    body = 'We are finding you another cab to {#dropStop#}.',
    updated_at = CURRENT_TIMESTAMP
WHERE key = 'SHARED_CAB_REASSIGNED'
  AND language = 'ENGLISH'
  AND body <> 'We are finding you another cab to {#dropStop#}.';

-- ── Driver push (atlas_driver_offer_bpp): the shared-cab allocation offer ──
-- A dedicated fcm_notification_type SHARED_CAB_ALLOCATION, so the driver gets the shared-cab card and NOT the taxi allocation
-- request. The shared-kernel FCMNotificationType must carry SHARED_CAB_ALLOCATION in the deployed build BEFORE this runs, else every
-- read of these rows fails to parse. trip_category is left NULL so the findMatchingMerchantPN fallback applies. Params, substituted
-- at send time: {#seats#}, {#vehicleNumber#}, {#boardStopCode#}, {#etaSeconds#}.
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'SHARED_CAB_ALLOCATION',
    'SHARED_CAB_ALLOCATION',
    moc.merchant_id,
    moc.id,
    'New shared-cab booking',
    '{#seats#} seat(s) booked on your shared cab {#vehicleNumber#}. Pick up at {#boardStopCode#} in about {#etaSeconds#}s.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
ON CONFLICT DO NOTHING;

-- Rows seeded with the first allocation copy; an edited body is left alone.
UPDATE atlas_driver_offer_bpp.merchant_push_notification
SET body = '{#seats#} seat(s) booked on your shared cab {#vehicleNumber#}. Pick up at {#boardStopCode#} in about {#etaSeconds#}s.',
    updated_at = CURRENT_TIMESTAMP
WHERE key = 'SHARED_CAB_ALLOCATION'
  AND language = 'ENGLISH'
  AND body = '{#seats#} seat(s) booked on your shared cab. Open the route screen for boarding details.';
