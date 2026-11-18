CREATE TABLE atlas_app.payout_lifecycle_event ();

ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN batch_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN batch_no text NOT NULL;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN driver_id text NOT NULL;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN event_type text NOT NULL;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN partner_code text ;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN payout_order_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN payout_request_id text ;
ALTER TABLE atlas_app.payout_lifecycle_event ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payout_lifecycle_event ADD PRIMARY KEY ( id);



------- SQL updates -------

CREATE INDEX CONCURRENTLY payout_lifecycle_event_idx_created_at_event_type_merchant_operating_city_id ON atlas_app.payout_lifecycle_event USING btree (created_at, event_type, merchant_operating_city_id);
ALTER TABLE atlas_app.payout_lifecycle_event ADD CONSTRAINT payout_lifecycle_event_unique_idx_event_type_payout_order_id UNIQUE (event_type, payout_order_id);