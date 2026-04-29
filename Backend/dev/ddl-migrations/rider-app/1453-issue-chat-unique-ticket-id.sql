

-- Add unique constraint on ticket_id
ALTER TABLE atlas_app.issue_chat
ADD CONSTRAINT unique_issue_chat_ticket_id UNIQUE (ticket_id);