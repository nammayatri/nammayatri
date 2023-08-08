ALTER TABLE atlas_app.person ADD COLUMN disability_id character varying(36);

CREATE TABLE atlas_app.disability_type (
  id character varying(36) NOT NULL PRIMARY KEY,
  tag character varying(255) NOT NULL,
  subtag character varying(255) NOT NULL,
  description character varying(1056),
  created_at timestamp with time zone DEFAULT now() NOT NULL
);