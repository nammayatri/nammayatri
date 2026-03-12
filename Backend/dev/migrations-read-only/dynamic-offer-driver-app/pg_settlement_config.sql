CREATE TABLE atlas_driver_offer_bpp.pg_settlement_config ();

ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN email_folder_name text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN email_imap_host text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN email_imap_port integer ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN email_password_encrypted text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN email_subject_filter text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN email_username text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN ingestion_enabled boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN last_ingestion_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN last_ingestion_status text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN payment_gateway text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN report_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN scheduled_hour_utc integer NOT NULL default 3;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN scheduled_minute_utc integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN sftp_file_pattern text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN sftp_host text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN sftp_password_encrypted text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN sftp_port integer ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN sftp_remote_path text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN sftp_username text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN source_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN status text NOT NULL default 'ACTIVE';
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN sftp_password text ;
ALTER TABLE atlas_driver_offer_bpp.pg_settlement_config ADD COLUMN email_password text ;


------- SQL updates -------

