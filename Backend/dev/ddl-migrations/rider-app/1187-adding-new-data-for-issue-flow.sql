

ALTER TABLE atlas_app.issue_message
ALTER COLUMN message TYPE character varying(1000);

ALTER TABLE atlas_app.issue_translation
ALTER COLUMN sentence TYPE character varying(1000) USING sentence::character varying(1000),
ALTER COLUMN translation TYPE character varying(1000) USING translation::character varying(1000);

ALTER TABLE atlas_app.issue_category
ADD COLUMN priority int NOT NULL DEFAULT 1;
--------------------------------------------------