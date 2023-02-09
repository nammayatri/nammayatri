ALTER TABLE atlas_app.call_status ADD COLUMN recording_url CHARACTER varying(255);
ALTER TABLE atlas_app.call_status ADD COLUMN conversation_duration int8;