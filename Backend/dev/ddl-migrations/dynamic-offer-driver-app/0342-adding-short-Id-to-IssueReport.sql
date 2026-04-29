ALTER TABLE atlas_driver_offer_bpp.issue_report ADD short_id character varying(36);
-- ONLY FOR LOCAL
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN kapture_disposition SET NOT NULL;

CREATE INDEX idx_issue_report_short_id ON atlas_driver_offer_bpp.issue_report USING btree (short_id);