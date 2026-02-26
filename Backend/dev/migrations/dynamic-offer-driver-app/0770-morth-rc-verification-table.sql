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