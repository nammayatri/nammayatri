-- only examples, do not run in prod
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
  'REFUND_REQUEST_APPROVED',
  'REFUND_REQUEST_APPROVED',
  moc.merchant_id,
  moc.id,
  'Refund Request Approved',
  'Your refund request for ride {#rideId#} was approved for amount of {#refundsAmount#} from total transaction amount {#transactionAmount#}.',
  'ENGLISH',
  CURRENT_TIMESTAMP,
  CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE city = 'Helsinki';

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
  'REFUND_REQUEST_REJECTED',
  'REFUND_REQUEST_REJECTED',
  moc.merchant_id,
  moc.id,
  'Refund Request Rejected',
  'Your refund request for ride {#rideId#} was rejected.',
  'ENGLISH',
  CURRENT_TIMESTAMP,
  CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE city = 'Helsinki';

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
  'REFUND_SUCCESS',
  'REFUND_SUCCESS',
  moc.merchant_id,
  moc.id,
  'Refund Processed',
  'Your refund for ride {#rideId#} is being processed for amount of {#refundsAmount#}.',
  'ENGLISH',
  CURRENT_TIMESTAMP,
  CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE city = 'Helsinki';

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
  'REFUND_FAILED',
  'REFUND_FAILED',
  moc.merchant_id,
  moc.id,
  'Refund Failed',
  'Your refund for ride {#rideId#} was failed.',
  'ENGLISH',
  CURRENT_TIMESTAMP,
  CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE city = 'Helsinki';
