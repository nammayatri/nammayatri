ALTER TABLE atlas_app.person ADD COLUMN device_id text ;

CREATE INDEX idx_device_id ON atlas_app.person USING btree (device_id);