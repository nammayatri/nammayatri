CREATE TABLE atlas_app.recon_utr_settlement ();

ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN bank_verified_amount numeric(30,2) ;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN bap_id text NOT NULL;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN bap_uri text NOT NULL;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN claimed_total_amount numeric(30,2) NOT NULL;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN deadline timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN deadline_breached_notified_at timestamp with time zone ;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN merchant_id text ;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN merchant_operating_city_id text ;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN resolution_status character varying(50) NOT NULL default 'RES_PENDING';
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN resolved_at timestamp with time zone ;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN resolved_by text ;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN send_attempts integer NOT NULL default 0;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN send_status character varying(50) NOT NULL default 'SEND_PENDING';
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN sent_at timestamp with time zone ;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN total_orders integer NOT NULL;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.recon_utr_settlement ADD COLUMN utr text NOT NULL;
ALTER TABLE atlas_app.recon_utr_settlement ADD PRIMARY KEY ( id);



------- SQL updates -------


--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---
ALTER TABLE atlas_app.recon_utr_settlement ALTER COLUMN send_status DROP NOT NULL;
ALTER TABLE atlas_app.recon_utr_settlement ALTER COLUMN send_attempts DROP NOT NULL;
ALTER TABLE atlas_app.recon_utr_settlement ALTER COLUMN deadline DROP NOT NULL;
--- Drop section ends. Please check before running ---



------- SQL updates -------


--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---
ALTER TABLE atlas_app.recon_utr_settlement ALTER COLUMN resolution_status DROP NOT NULL;
--- Drop section ends. Please check before running ---



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

