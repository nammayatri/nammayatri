ALTER TABLE
  atlas_app.person DROP CONSTRAINT unique_email;

ALTER TABLE
  atlas_app.person DROP COLUMN IF EXISTS email;

ALTER TABLE
  atlas_app.person
ADD
  COLUMN email_encrypted character varying(255) UNIQUE,
ADD
  COLUMN email_hash bytea UNIQUE;
