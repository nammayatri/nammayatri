ALTER TABLE atlas_transporter.call_status ADD COLUMN recording_url CHARACTER varying(255);
ALTER TABLE atlas_transporter.call_status ADD COLUMN conversation_duration int8;