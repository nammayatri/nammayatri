--Why it disapeared?
ALTER TABLE atlas_transporter.job RENAME TO scheduler_job;
----------------------------------------------------------------

CREATE TABLE atlas_transporter.merchant_message (
    merchant_id character(36) NOT NULL REFERENCES atlas_transporter.merchant (id),
    message_key character varying(255) NOT NULL,
    message character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (merchant_id, message_key)
);

WITH MerchantMessages AS (
  SELECT T1.id, 'SEND_OTP', '<#> Your OTP for login to Yatri App is {#otp#} {#hash#}'
  FROM atlas_transporter.merchant AS T1
)
INSERT INTO atlas_transporter.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.id, 'WELCOME_TO_PLATFORM', 'Welcome to the Yatri platform! Your agency ({#orgName#}) has added you as a driver. Start getting rides by installing the app: {#link#}'
  FROM atlas_transporter.merchant AS T1
)
INSERT INTO atlas_transporter.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);