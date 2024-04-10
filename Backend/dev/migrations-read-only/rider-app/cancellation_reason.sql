CREATE TABLE atlas_app.cancellation_reason ();

ALTER TABLE atlas_app.cancellation_reason ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN on_assign boolean NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN on_confirm boolean NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN on_init boolean NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN on_search boolean NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN priority integer NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN reason_code text NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cancellation_reason ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cancellation_reason ADD PRIMARY KEY ( reason_code);


------- SQL updates -------

ALTER TABLE atlas_app.cancellation_reason ALTER COLUMN updated_at DROP NOT NULL;
ALTER TABLE atlas_app.cancellation_reason ALTER COLUMN created_at DROP NOT NULL;