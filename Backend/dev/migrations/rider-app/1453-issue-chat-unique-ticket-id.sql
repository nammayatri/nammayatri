-- Add unique constraint on ticket_id to prevent duplicate issue chats
-- First remove any duplicate rows if they exist
WITH duplicates AS (
    SELECT ticket_id, MIN(id) as keep_id
    FROM atlas_app.issue_chat
    GROUP BY ticket_id
    HAVING COUNT(*) > 1
)
DELETE FROM atlas_app.issue_chat
WHERE id NOT IN (SELECT keep_id FROM duplicates)
AND ticket_id IN (SELECT ticket_id FROM duplicates);

-- Add unique constraint on ticket_id
ALTER TABLE atlas_app.issue_chat
ADD CONSTRAINT unique_issue_chat_ticket_id UNIQUE (ticket_id);