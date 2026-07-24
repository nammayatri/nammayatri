-- Backs the post-resolution satisfaction-prompt flow:
-- 1. issue_config.on_customer_not_satisfied_msgs: messages posted to chat when the
--    customer answers "Not satisfied" to the satisfaction prompt (typically a single
--    REOPEN_PROMPT message). Defaults to empty array so existing merchants keep working.
-- 2. issue_report.customer_response: persists the customer's latest reply to the
--    satisfaction prompt (ACCEPT / ESCALATE) so analytics can attribute closes/reopens.
ALTER TABLE atlas_app.issue_config
  ADD COLUMN on_customer_not_satisfied_msgs text[] NOT NULL DEFAULT '{}';

ALTER TABLE atlas_app.issue_report
  ADD COLUMN customer_response text;
