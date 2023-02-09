ALTER TABLE atlas_transporter.organization ADD COLUMN origin_restriction text[];
ALTER TABLE atlas_transporter.organization ADD COLUMN destination_restriction text[];

UPDATE atlas_transporter.organization
SET
  origin_restriction = '{"Ernakulam"}',
  destination_restriction = '{"Ernakulam", "Kerala"}';


ALTER TABLE atlas_transporter.organization ALTER COLUMN origin_restriction SET NOT NULL;
ALTER TABLE atlas_transporter.organization ALTER COLUMN destination_restriction SET NOT NULL;