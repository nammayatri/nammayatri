CREATE TABLE atlas_app.cancellation_reason ();

ALTER TABLE atlas_app.cancellation_reason ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN description character varying(255) NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN on_assign boolean NOT NULL default true;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN on_confirm boolean NOT NULL default true;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN on_init boolean NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN on_search boolean NOT NULL default true;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN priority smallint NOT NULL default 0;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN reason_code character varying(255) NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN updated_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cancellation_reason ADD PRIMARY KEY ( reason_code);