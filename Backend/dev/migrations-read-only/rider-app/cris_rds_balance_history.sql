CREATE TABLE atlas_app.cris_rds_balance_history ();

ALTER TABLE atlas_app.cris_rds_balance_history ADD COLUMN balance double precision NOT NULL;
ALTER TABLE atlas_app.cris_rds_balance_history ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cris_rds_balance_history ADD COLUMN execution_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.cris_rds_balance_history ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.cris_rds_balance_history ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.cris_rds_balance_history ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cris_rds_balance_history ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.cris_rds_balance_history ADD COLUMN date_ist date ;