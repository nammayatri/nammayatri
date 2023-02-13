CREATE TABLE atlas_driver_offer_bpp.media_file (
  id character(36) NOT NULL,
  type character(36) NOT NULL,
  url text NOT NULL,
  created_at timestamp NOT NULL,
  PRIMARY KEY (id)
);


CREATE TABLE atlas_driver_offer_bpp.message (
  id character(36) NOT NULL,
  type character(100) NOT NULL,
  title character varying(255) NOT NULL,
  description text NOT NULL,
  media_files text[][],
  merchant_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id),
  created_at timestamp NOT NULL,
  PRIMARY KEY (id)
);


CREATE TABLE atlas_driver_offer_bpp.message_translation (
  message_id character(36) REFERENCES atlas_driver_offer_bpp.message (id),
  language character(36) NOT NULL,
  title character varying(255) NOT NULL,
  description text NOT NULL,
  created_at timestamp NOT NULL,
  PRIMARY KEY (message_id, language)
);


CREATE TABLE atlas_driver_offer_bpp.message_report (
  message_id character(36) REFERENCES atlas_driver_offer_bpp.message (id),
  driver_id character(36) REFERENCES atlas_driver_offer_bpp.person (id),
  delivery_status character(36) NOT NULL,
  read_status boolean NOT NULL,
  reply text,
  message_dynamic_fields JSON,
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL,
  PRIMARY KEY (message_id, driver_id)
);
