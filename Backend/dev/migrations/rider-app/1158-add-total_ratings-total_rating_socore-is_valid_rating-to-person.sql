ALTER TABLE atlas_app.person ADD COLUMN total_ratings int NOT NULL DEFAULT 0;
ALTER TABLE atlas_app.person ADD COLUMN total_rating_score int NOT NULL DEFAULT 0;
ALTER TABLE atlas_app.person ADD COLUMN is_valid_rating boolean NOT NULL DEFAULT FALSE;

