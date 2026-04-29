ALTER TABLE atlas_bap_dashboard.transaction ALTER COLUMN requestor_id DROP NOT NULL;

ALTER TABLE atlas_bap_dashboard.transaction ADD COLUMN server_name character varying(255);
