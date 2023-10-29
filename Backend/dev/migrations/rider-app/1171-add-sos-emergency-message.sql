WITH MerchantMessages AS (
  SELECT T1.id, 'SEND_SOS_ALERT', '{#userName#} has activated SOS/ emergency during NammaYatri ride. Track the ride here {#rideLink#} -Juspay'
  FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.id, 'MARK_RIDE_AS_SAFE', '{#userName#} has confirmed their safety during the Namma Yatri ride - Juspay.'
  FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);

CREATE TABLE atlas_app.sos_media (
    id character(36) NOT NULL,
    type character(36) NOT NULL,
    url text NOT NULL,
    created_at timestamp NOT NULL,
    PRIMARY KEY (id)
);

ALTER TABLE atlas_app.sos ADD COLUMN ticket_id text;

ALTER TABLE atlas_app.person ADD COLUMN share_emergency_contacts boolean NOT NULL default false,
  ADD COLUMN trigger_ny_support boolean NOT NULL default true,
  ADD COLUMN night_safety_checks boolean NOT NULL default true,
  ADD COLUMN has_completed_safety_setup boolean NOT NULL default false;

UPDATE atlas_app.merchant_service_config
SET config_json =
  json_build_object(
    'createAuth', '0.1.0|2|JMM1g9qYbwWtoscHG4qTUql80FhbCz8dMpB0ZMWaUhbSa9RBqO3bUsggftvSo2kCHjJTYgGykwXJzU1xVjDu',
    'updateAuth', '0.1.0|2|JMM1g9qYbwWtoscHG4qTUql80FhbCz8dMpB0ZMWaUhbSa9RBqO3bUsggftvSo2kCHjJTYgGykwXJzU1xVjDu',
    'version', 'v.2.0',
    'url', 'nammayatri.kapturecrm.com'
    )
WHERE merchant_service_config.service_name='Ticket_Kapture';
ALTER TABLE atlas_app.merchant ADD COLUMN tracking_short_url_pattern text DEFAULT 'nammayatri.in/t/' NOT NULL;
