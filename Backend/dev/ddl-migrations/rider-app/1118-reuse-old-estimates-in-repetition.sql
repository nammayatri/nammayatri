

CREATE INDEX idx_driver_offer_s_req_id ON atlas_app.driver_offer USING btree (estimate_id);

ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_enabled;
ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_enabled_v2;
ALTER TABLE atlas_app.estimate DROP COLUMN auto_assign_quote_id;
