-- Per-message auto reply for CREATE_TICKET / AUTO_CREATE_TICKET / FEEDBACK_TICKET messages (configured in the message editor).
ALTER TABLE atlas_app.issue_message ADD COLUMN IF NOT EXISTS on_submit_reply_msgs text[];
