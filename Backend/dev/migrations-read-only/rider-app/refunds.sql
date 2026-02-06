CREATE TABLE atlas_app.refunds ();

ALTER TABLE atlas_app.refunds ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.refunds ADD COLUMN error_code text ;
ALTER TABLE atlas_app.refunds ADD COLUMN error_message text ;
ALTER TABLE atlas_app.refunds ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.refunds ADD COLUMN id_assigned_by_service_provider text ;
ALTER TABLE atlas_app.refunds ADD COLUMN initiated_by text ;
ALTER TABLE atlas_app.refunds ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.refunds ADD COLUMN order_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.refunds ADD COLUMN refund_amount double precision NOT NULL;
ALTER TABLE atlas_app.refunds ADD COLUMN short_id text NOT NULL;
ALTER TABLE atlas_app.refunds ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.refunds ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.refunds ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.refunds ADD COLUMN split json ;


------- SQL updates -------

ALTER TABLE atlas_app.refunds ALTER COLUMN short_id TYPE character varying(36);


------- SQL updates -------

ALTER TABLE atlas_app.refunds ADD COLUMN is_api_call_success boolean ;




------- SQL updates -------

ALTER TABLE atlas_app.refunds ADD COLUMN arn text ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.refunds ADD COLUMN completed_at timestamp with time zone ;