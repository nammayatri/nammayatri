-- R17 arrival popup: SHARED_CAB_ARRIVING now carries a boarding countdown ({#boardDeadlineSec#},
-- {#vehicleLast4#} -- see SharedLogic.SharedCab.Notify.notifyArriving), and SHARED_CAB_REASSIGNED's
-- copy is reworded to also read right for the R17 "missed the cab" timeout case (0067 wrote it for
-- SEAT_LOST only). Idempotent: safe to re-run.

-- Existing rows from 0067 (this is an UPDATE, not an INSERT -- those rows already exist). ENGLISH only, like 0067:
-- other languages' copy is theirs to sign off. No city scoping: 0067 seeded exactly the shared-cab cities.
UPDATE atlas_app.merchant_push_notification
SET title = 'Your cab is here',
    body = 'Cab {#vehicleLast4#} is at {#boardStop#}. Board within {#boardDeadlineSec#}s and enter the code inside.',
    updated_at = CURRENT_TIMESTAMP
WHERE key = 'SHARED_CAB_ARRIVING'
  AND language = 'ENGLISH';

UPDATE atlas_app.merchant_push_notification
SET title = 'You missed your cab',
    body = 'We are finding you another cab to {#dropStop#}.',
    updated_at = CURRENT_TIMESTAMP
WHERE key = 'SHARED_CAB_REASSIGNED'
  AND language = 'ENGLISH';

-- Cities onboarded after 0067 ran (same WHERE EXISTS / NOT EXISTS guard as 0067).
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
    ('SHARED_CAB_ARRIVING', 'Your cab is here', 'Cab {#vehicleLast4#} is at {#boardStop#}. Board within {#boardDeadlineSec#}s and enter the code inside.'),
    ('SHARED_CAB_REASSIGNED', 'You missed your cab', 'We are finding you another cab to {#dropStop#}.')
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
