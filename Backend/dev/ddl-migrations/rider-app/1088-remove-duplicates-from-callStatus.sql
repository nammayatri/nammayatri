

ALTER TABLE atlas_app.call_status ADD CONSTRAINT unique_call_sid UNIQUE (call_id);
