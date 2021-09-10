ALTER TABLE atlas_app.cancellation_reason ADD COLUMN on_search bool DEFAULT True NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN on_confirm bool DEFAULT True NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN on_assign bool DEFAULT True NOT NULL;