ALTER TABLE atlas_driver_offer_bpp.person
    ADD COLUMN unencrypted_alternate_mobile_number character varying(255);

ALTER TABLE atlas_driver_offer_bpp.person
    ADD COLUMN alternate_mobile_number_encrypted character varying(255);

ALTER TABLE atlas_driver_offer_bpp.person
    ADD COLUMN alternate_mobile_number_hash bytea;

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message)
VALUES ('nearest-drivers-testing-organization', 'ALTERNATE_NUMBER_OTP', '{#otp#} is your OTP for adding alternate number in Namma Yatri App. {#hash#}');

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message)
VALUES ('favorit0-0000-0000-0000-00000favorit', 'ALTERNATE_NUMBER_OTP', '{#otp#} is your OTP for adding alternate number in Namma Yatri App. {#hash#}');

ALTER TABLE atlas_driver_offer_bpp.registration_token
    ADD COLUMN alternate_number_attempts bigint DEFAULT 5 NOT NULL;

