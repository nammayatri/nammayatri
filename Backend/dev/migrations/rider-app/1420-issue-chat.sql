ALTER TABLE atlas_app.issue_report
ALTER COLUMN category_id DROP NOT NULL;

CREATE TABLE IF NOT EXISTS atlas_app.issue_chat (
  id character varying(36) PRIMARY KEY NOT NULL,
  ticket_id character varying(255) NOT NULL,
  ride_id character varying(36) REFERENCES atlas_app.ride(id),
  person_id character varying(36) NOT NULL REFERENCES atlas_app.person(id),
  chats text[] NOT NULL,
  media_files text[] NOT NULL,
  issue_report_id character varying(36) REFERENCES atlas_app.issue_report(id),
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL
);

CREATE INDEX idx_issue_chat_ticket_id ON atlas_app.issue_chat USING btree (ticket_id);
