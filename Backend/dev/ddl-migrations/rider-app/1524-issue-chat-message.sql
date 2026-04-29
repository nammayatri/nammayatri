-- Live chat table for rider <-> dashboard operator messaging,
-- scoped to an IssueReport. Separate from the existing `comment` table
-- which serves as an internal operator audit trail.

CREATE TABLE IF NOT EXISTS atlas_app.chat_message (
    id character(36) NOT NULL PRIMARY KEY,
    issue_report_id character(36) NOT NULL,
    sender_id character(36) NOT NULL,
    sender_type text NOT NULL,
    chat_content_type text NOT NULL DEFAULT 'CHAT_TEXT',
    message text NOT NULL DEFAULT '',
    media_file_ids text[] NOT NULL DEFAULT '{}',
    read_at timestamp,
    created_at timestamp NOT NULL DEFAULT now(),
    merchant_id character(36)
);

CREATE INDEX IF NOT EXISTS idx_chat_message_issue_created
    ON atlas_app.chat_message (issue_report_id, created_at);

CREATE INDEX IF NOT EXISTS idx_chat_message_unread
    ON atlas_app.chat_message (issue_report_id)
    WHERE read_at IS NULL;
