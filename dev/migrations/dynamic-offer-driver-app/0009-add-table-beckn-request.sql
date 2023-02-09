CREATE TABLE atlas_driver_offer_bpp.beckn_request (
  id character varying(36) PRIMARY KEY,
  beckn_request text NOT NULL,
  signature_header varchar(255) NOT NULL,
  time_stamp timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
);
