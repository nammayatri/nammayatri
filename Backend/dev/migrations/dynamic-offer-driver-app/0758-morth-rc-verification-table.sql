-- Create morth_rc_verification table to store responses from the MoRTH Parivahan API
CREATE TABLE atlas_driver_offer_bpp.morth_rc_verification (
  id                        character varying(36)  NOT NULL,
  driver_id                 character varying(36)  NOT NULL,
  rc_number                 character varying(20)  NOT NULL,
  success                   boolean                NOT NULL,
  rc_validity               character varying(50),
  rc_valid_upto             character varying(50),
  message                   text,
  status_code               integer,
  merchant_id               character varying(36),
  merchant_operating_city_id character varying(36),
  created_at                timestamp with time zone NOT NULL default CURRENT_TIMESTAMP,
  updated_at                timestamp with time zone NOT NULL default CURRENT_TIMESTAMP,
  PRIMARY KEY (id)
);

CREATE INDEX idx_morth_rc_verification_driver_id  ON atlas_driver_offer_bpp.morth_rc_verification USING btree (driver_id);
CREATE INDEX idx_morth_rc_verification_rc_number  ON atlas_driver_offer_bpp.morth_rc_verification USING btree (rc_number);

INSERT INTO atlas_driver_offer_bpp.merchant_service_config (
  merchant_id,
  service_name,
  merchant_operating_city_id,
  config_json,
  created_at,
  updated_at
) VALUES (
  'favorit0-0000-0000-0000-00000favorit',
  'Verification_Morth',
  'favorit0-0000-0000-0000-00000000city',
  '{
    "url": "<url>",
    "apiKey": "<apiKey>"
  }',
  now(),
  now()
)
ON CONFLICT DO NOTHING;

-- To just update the verification_providers_priority_list, run the following query:
-- UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
-- SET verification_providers_priority_list = '{"Morth"}';