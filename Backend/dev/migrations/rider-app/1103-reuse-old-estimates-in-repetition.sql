ALTER TABLE atlas_app.estimate ADD COLUMN bpp_estimate_id character(36);
UPDATE atlas_app.estimate AS T1 SET bpp_estimate_id = 'UNKNOWN';
ALTER TABLE atlas_app.estimate ALTER COLUMN bpp_estimate_id SET NOT NULL;

ALTER TABLE atlas_app.search_request ADD COLUMN auto_assign_enabled boolean;
UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled = False;
ALTER TABLE atlas_app.search_request ALTER COLUMN auto_assign_enabled SET NOT NULL;

ALTER TABLE atlas_app.search_request ADD COLUMN auto_assign_enabled_v2 boolean;
UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled_v2 = False;
ALTER TABLE atlas_app.search_request ALTER COLUMN auto_assign_enabled_v2 SET NOT NULL;

ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_enabled;
ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_enabled_v2;
ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_quote_id;

UPDATE atlas_app.person_flow_status SET flow_status = '{"tag":"IDLE"}';