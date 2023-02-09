ALTER TABLE atlas_app.merchant ADD COLUMN origin_restriction text[];
ALTER TABLE atlas_app.merchant ADD COLUMN destination_restriction text[];

-- local testing data
UPDATE atlas_app.merchant
SET
  origin_restriction = '{"Ernakulam"}',
  destination_restriction = '{"Ernakulam", "Kerala"}'
WHERE id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';
