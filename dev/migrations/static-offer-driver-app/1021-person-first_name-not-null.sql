UPDATE atlas_transporter.person SET first_name = 'UNKNOWN'
  WHERE first_name ISNULL;

ALTER TABLE atlas_transporter.person ALTER COLUMN first_name SET NOT NULL;
