-- End-ride financial communication copy for merchant_push_notification (keys used in Domain.Action.UI.Ride.EndRide.Internal).
-- Template variable: {#balance#} where applicable.

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIGGER_SERVICE',
    'FIN_FLEET_SUBSCRIPTION_BLOCKING',
    moc.merchant_id,
    moc.id,
    'Subscription Balance Critical!',
    'Your fleet subscription balance is too low. Please recharge immediately to continue taking rides.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIGGER_SERVICE',
    'FIN_FLEET_SUBSCRIPTION_LOW_WARNING',
    moc.merchant_id,
    moc.id,
    'Low Balance Alert!',
    'Your fleet subscription balance is Rs.{#balance#}. This may block future rides. Please recharge soon.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIGGER_SERVICE',
    'FIN_DRIVER_SUBSCRIPTION_BLOCKING',
    moc.merchant_id,
    moc.id,
    'Subscription Balance Critical!',
    'Your subscription balance is too low. Please recharge immediately to continue taking rides.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIGGER_SERVICE',
    'FIN_DRIVER_SUBSCRIPTION_LOW_WARNING',
    moc.merchant_id,
    moc.id,
    'Low Balance Alert!',
    'Your subscription balance is Rs.{#balance#}. This may block future rides. Please recharge soon.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIGGER_SERVICE',
    'FIN_EARNINGS_BLOCKING',
    moc.merchant_id,
    moc.id,
    'Cash Rides Blocked',
    'Your earnings balance (Rs.{#balance#}) is insufficient for cash rides.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIGGER_SERVICE',
    'FIN_EARNINGS_LOW_WARNING',
    moc.merchant_id,
    moc.id,
    'Earnings Balance Low',
    'Your earnings balance is Rs.{#balance#}. Cash rides may be blocked soon.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.merchant_operating_city moc;
