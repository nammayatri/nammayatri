ALTER TABLE atlas_app.gate_info ADD COLUMN trigger_notify_retry_interval_sec integer;
ALTER TABLE atlas_app.gate_info ADD COLUMN trigger_notify_max_retry_duration_sec integer;
