CREATE TABLE atlas_driver_offer_bpp.issue_category (
  id character varying(255) NOT NULL,
  category character varying(255) NOT NULL,
  logo_url character varying(255) NOT NULL,
  CONSTRAINT issue_category_pkey PRIMARY KEY (id)
);

CREATE TABLE atlas_driver_offer_bpp.issue_option (
  id character varying(255) NOT NULL,
  issue_category_id character varying(255) NOT NULL,
  option character varying(255) NOT NULL,
  CONSTRAINT issue_option_pkey PRIMARY KEY (id)
);

CREATE TABLE atlas_driver_offer_bpp.issue_translation (
  id character varying(255) NOT NULL,
  sentence character varying(255) NOT NULL,
  translation character varying(255) NOT NULL,
  language character varying(255) NOT NULL,
  CONSTRAINT issue_translation_pkey PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.issue_report (
  id character varying(255) PRIMARY KEY NOT NULL,
  driver_id character varying(255) NOT NULL REFERENCES atlas_driver_offer_bpp.person (id),
  ride_id character varying(255) REFERENCES atlas_driver_offer_bpp.ride (id),
  description character varying(255) NOT NULL,
  assignee character varying(255),
  status character varying(255) NOT NULL,
  category character varying(255) NOT NULL,
  option character varying(255),
  deleted boolean,
  media_files text[][],
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL
);

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.comment (
  id character varying(255) PRIMARY KEY NOT NULL,
  issue_report_id character varying(255) NOT NULL REFERENCES atlas_driver_offer_bpp.issue_report (id),
  author character varying(255) NOT NULL,
  comment character varying(255) NOT NULL,
  created_at timestamp NOT NULL
);