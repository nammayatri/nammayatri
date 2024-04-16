UPDATE atlas_app.person SET blocked = false;

ALTER TABLE atlas_app.person ALTER COLUMN blocked SET NOT NULL;
