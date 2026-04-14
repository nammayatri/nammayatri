-- Live chat table for driver <-> dashboard operator messaging,
-- scoped to an IssueReport. Mirrors the rider-app schema so
-- shared-services Beam / queries are homogeneous.

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.chat_message (
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
    ON atlas_driver_offer_bpp.chat_message (issue_report_id, created_at);

CREATE INDEX IF NOT EXISTS idx_chat_message_unread
    ON atlas_driver_offer_bpp.chat_message (issue_report_id)
    WHERE read_at IS NULL;
