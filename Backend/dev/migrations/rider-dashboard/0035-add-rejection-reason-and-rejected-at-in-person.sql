ALTER table atlas_bap_dashboard.person ADD COLUMN rejection_reason character varying(255);
ALTER table atlas_bap_dashboard.person ADD COLUMN rejected_at timestamp with time zone;