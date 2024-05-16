UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled = (
    SELECT auto_assign_enabled AS T2 FROM atlas_app.estimate AS T2
    WHERE T1.id = T2.request_id AND T2.auto_assign_enabled = true
    LIMIT 1)
  WHERE T1.created_at > now () - interval '6 hour';
UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled = false
  WHERE T1.created_at > now () - interval '6 hour' AND T1.auto_assign_enabled IS NULL;

UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled_v2 = (
    SELECT auto_assign_enabled_v2 AS T2 FROM atlas_app.estimate AS T2
    WHERE T1.id = T2.request_id AND T2.auto_assign_enabled_v2 = true
    LIMIT 1)
  WHERE T1.created_at > now () - interval '6 hour';
UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled_v2 = false
  WHERE T1.created_at > now () - interval '6 hour' AND T1.auto_assign_enabled_v2 IS NULL;

CREATE INDEX idx_driver_offer_s_req_id ON atlas_app.driver_offer USING btree (estimate_id);

-------------------------------------------------------------------------------------------
-------------------------------AFTER_FULL_ROLL_OUT-----------------------------------------
-------------------------------------------------------------------------------------------

UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled = (
    SELECT auto_assign_enabled AS T2 FROM atlas_app.estimate AS T2
    WHERE T1.id = T2.request_id AND T2.auto_assign_enabled = true
    LIMIT 1)
  WHERE T1.auto_assign_enabled IS NULL;
UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled = false
  WHERE T1.auto_assign_enabled IS NULL;

UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled_v2 = (
    SELECT auto_assign_enabled_v2 AS T2 FROM atlas_app.estimate AS T2
    WHERE T1.id = T2.request_id AND T2.auto_assign_enabled_v2 = true
    LIMIT 1)
  WHERE T1.auto_assign_enabled_v2 IS NULL;
UPDATE atlas_app.search_request AS T1 SET auto_assign_enabled_v2 = false
  WHERE T1.auto_assign_enabled_v2 IS NULL;

ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_enabled;
ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_enabled_v2;
ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_quote_id;
