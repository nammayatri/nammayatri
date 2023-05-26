ALTER TABLE atlas_app.search_request ADD COLUMN auto_assign_enabled boolean;
UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled = (SELECT auto_assign_enabled AS T2 FROM atlas_app.estimate AS T2 WHERE T1.id = T2.request_id)
  WHERE T1.created_at > now () - interval '6 hour';

ALTER TABLE atlas_app.search_request ADD COLUMN auto_assign_enabled_v2 boolean;
UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled_v2 = (SELECT auto_assign_enabled_v2 AS T2 FROM atlas_app.estimate AS T2 WHERE T1.id = T2.request_id)
  WHERE T1.created_at > now () - interval '6 hour';

-------------------------------------------------------------------------------------------
-------------------------------AFTER_FULL_ROLL_OUT-----------------------------------------
-------------------------------------------------------------------------------------------

UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled = (SELECT auto_assign_enabled AS T2 FROM atlas_app.estimate AS T2 WHERE T1.id = T2.request_id)
  WHERE T1.auto_assign_enabled IS NULL;

UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled_v2 = (SELECT auto_assign_enabled_v2 AS T2 FROM atlas_app.estimate AS T2 WHERE T1.id = T2.request_id)
  WHERE T1.auto_assign_enabled_v2 IS NULL;

ALTER TABLE atlas_app.search_request ALTER COLUMN auto_assign_enabled SET NOT NULL;
ALTER TABLE atlas_app.search_request ALTER COLUMN auto_assign_enabled_v2 SET NOT NULL;

ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_enabled;
ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_enabled_v2;
ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_quote_id;
