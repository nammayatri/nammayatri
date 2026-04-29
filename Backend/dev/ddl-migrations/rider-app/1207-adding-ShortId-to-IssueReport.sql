ALTER TABLE atlas_app.issue_report ADD short_id character varying(36);

CREATE INDEX idx_issue_report_short_id ON atlas_app.issue_report USING btree (short_id);