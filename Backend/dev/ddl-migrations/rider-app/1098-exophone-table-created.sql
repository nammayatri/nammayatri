

ALTER TABLE atlas_app.merchant DROP COLUMN exo_phones;

-- updating after dsl generated query
ALTER TABLE atlas_app.booking ALTER COLUMN primary_exophone DROP NOT NULL;
ALTER TABLE atlas_app.booking ALTER COLUMN primary_exophone SET NOT NULL;

