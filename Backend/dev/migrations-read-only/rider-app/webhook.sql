CREATE TABLE atlas_app.webhook ();

ALTER TABLE atlas_app.webhook ADD COLUMN batch_id text NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN city text NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.webhook ADD COLUMN event_name text NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN ext_merchant_name text NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN last_tried_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN mode text NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN response_code text ;
ALTER TABLE atlas_app.webhook ADD COLUMN response_message text ;
ALTER TABLE atlas_app.webhook ADD COLUMN retry_count integer NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN webhook_data text NOT NULL;
ALTER TABLE atlas_app.webhook ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.webhook ADD PRIMARY KEY ( id);
