
ALTER TABLE atlas_app.search_request ADD COLUMN disability_tag character(255);

ALTER TABLE atlas_app.person ADD COLUMN has_disability boolean DEFAULT NULL;