CREATE TABLE atlas_app.issue_category (
  id character varying(255) NOT NULL,
  category character varying(255) NOT NULL,
  logo_url character varying(255) NOT NULL,
  CONSTRAINT issue_category_pkey PRIMARY KEY (id)
);

CREATE TABLE atlas_app.issue_option (
  id character varying(255) NOT NULL,
  issue_category_id character varying(255),
  issue_message_id character varying(255) NOT NULL,
  option character varying(255) NOT NULL,
  label text,
  priority int NOT NULL,
  CONSTRAINT issue_option_pkey PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS atlas_app.issue_report (
  id character varying(255) PRIMARY KEY NOT NULL,
  person_id character varying(255) NOT NULL REFERENCES atlas_app.person (id),
  ride_id character varying(255) REFERENCES atlas_app.ride (id),
  description character varying(255) NOT NULL,
  assignee character varying(255),
  status character varying(255) NOT NULL,
  category_id character varying(255) NOT NULL REFERENCES atlas_app.issue_category(id),
  option_id character varying(255) REFERENCES atlas_app.issue_option(id),
  deleted boolean,
  media_files text[][],
  ticket_id character varying(255),
  chats text[],
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL
);

CREATE TABLE IF NOT EXISTS atlas_app.comment (
  id character varying(255) PRIMARY KEY NOT NULL,
  issue_report_id character varying(255) NOT NULL REFERENCES atlas_app.issue_report (id),
  author_id character varying(255) NOT NULL,
  comment character varying(255) NOT NULL,
  created_at timestamp NOT NULL
);

CREATE TABLE atlas_app.issue_translation (
  id character varying(255) NOT NULL,
  sentence character varying(255) NOT NULL,
  translation character varying(255) NOT NULL,
  language character varying(255) NOT NULL,
  CONSTRAINT issue_translation_pkey PRIMARY KEY (id)
);

CREATE TABLE atlas_app.media_file (
  id character(36) NOT NULL,
  type character(36) NOT NULL,
  url text NOT NULL,
  created_at timestamp NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE atlas_app.issue_message (
  id character(36) NOT NULL PRIMARY KEY,
  option_id character(36) REFERENCES atlas_app.issue_option (id),
  category_id character(36) REFERENCES atlas_app.issue_category (id),
  message character varying(255) NOT NULL,
  label text,
  priority int NOT NULL
);

CREATE TABLE atlas_app.issue_config (
  id character(36) NOT NULL PRIMARY KEY,
  auto_mark_issue_closed_duration double precision,
  on_auto_mark_issue_cls_msgs text[],
  on_create_issue_msgs text[],
  on_issue_reopen_msgs text[],
  on_kapt_mark_issue_res_msgs text[]
);

--ADDING DRIVER_ID COLUMN FOR BACKWARD COMPATIBILITY
ALTER TABLE atlas_app.issue_report ADD COLUMN driver_id character(36);
