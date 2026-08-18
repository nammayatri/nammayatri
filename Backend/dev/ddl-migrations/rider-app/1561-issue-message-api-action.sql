-- ApiCall lookup step config for the in-house chat decision tree. JSON-encoded
-- IssueManagement.Domain.Types.Issue.IssueMessage.ApiCallAction; set only on
-- messages with message_type = 'ApiCall'. NULL for every ordinary message.
ALTER TABLE atlas_app.issue_message
  ADD COLUMN IF NOT EXISTS api_action text;
