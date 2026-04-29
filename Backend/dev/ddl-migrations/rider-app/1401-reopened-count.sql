ALTER TABLE atlas_app.issue_config ADD COLUMN on_issue_close_msgs text[] DEFAULT '{}';
ALTER TABLE atlas_app.issue_config ADD COLUMN reopen_count int DEFAULT 0;
ALTER TABLE atlas_app.issue_report ADD COLUMN reopened_count int;