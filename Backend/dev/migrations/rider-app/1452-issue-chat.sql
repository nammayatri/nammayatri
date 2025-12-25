ALTER TABLE atlas_app.issue_chat ADD COLUMN kapture_data TEXT;

CREATE INDEX idx_issue_chat_person_id ON atlas_app.issue_chat USING btree (person_id);
