ALTER TABLE atlas_app.estimate ADD COLUMN bpp_estimate_id character(36) DEFAULT 'UNKNOWN' NOT NULL;
ALTER TABLE atlas_app.estimate ALTER COLUMN bpp_estimate_id DROP DEFAULT;

UPDATE atlas_app.person_flow_status SET flow_status = '{"status":"IDLE"}' WHERE (flow_status->>'status') = 'GOT_ESTIMATE';