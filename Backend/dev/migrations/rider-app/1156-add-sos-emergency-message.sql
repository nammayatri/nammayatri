WITH MerchantMessages AS (
  SELECT T1.id, 'SEND_SOS_ALERT', '{#userName#} has activated SOS/ emergency during NammaYatri ride. Track the ride here {#rideLink#} -Juspay'
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

ALTER TABLE atlas_app.merchant ADD COLUMN media_file_url_pattern text DEFAULT 'http://localhost:8016/ui/<DOMAIN>/media?filePath=<FILE_PATH>' NOT NULL;

ALTER TABLE atlas_app.person ADD COLUMN share_emergency_contacts boolean NOT NULL,
  ADD COLUMN trigger_ny_support boolean NOT NULL,
  ADD COLUMN night_safety_checks boolean NOT NULL,
  ADD COLUMN has_completed_safety_setup boolean NOT NULL;
