

ALTER TABLE atlas_driver_offer_bpp.issue_report ADD COLUMN ticket_id character varying(255);

CREATE INDEX idx_ticket_id ON atlas_driver_offer_bpp.issue_report USING btree (ticket_id);
