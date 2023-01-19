ALTER TABLE atlas_app.merchant ADD COLUMN origin_restriction text[];
ALTER TABLE atlas_app.merchant ADD COLUMN destination_restriction text[];

-- local testing data
UPDATE atlas_app.merchant
SET
  origin_restriction = '{"Ernakulam"}',
  destination_restriction = '{"Ernakulam", "Kerala"}';
