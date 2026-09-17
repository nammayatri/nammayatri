-- An unsolicited on_issue has to be addressed somewhere. Every callback today is built
-- from the inbound request's context, so nothing about the BAP is persisted: bap_uri is
-- not stored at all, and transaction_id holds an internally generated GUID rather than
-- the Beckn thread id. Both are needed to push a callback after a dashboard-side change.
ALTER TABLE atlas_app.igm_issue ADD COLUMN bap_uri text;
ALTER TABLE atlas_app.igm_issue ADD COLUMN beckn_transaction_id text;
