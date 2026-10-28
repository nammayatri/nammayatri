CREATE TABLE atlas_driver_offer_bpp.mg_eligibility ();

ALTER TABLE atlas_driver_offer_bpp.mg_eligibility ADD COLUMN campaign_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_eligibility ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_eligibility ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_eligibility ADD COLUMN invited_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_eligibility ADD COLUMN notified_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.mg_eligibility ADD COLUMN reason text ;
ALTER TABLE atlas_driver_offer_bpp.mg_eligibility ADD COLUMN source text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_eligibility ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.mg_eligibility ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.mg_eligibility ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY mg_eligibility_idx_campaign_id ON atlas_driver_offer_bpp.mg_eligibility USING btree (campaign_id);
ALTER TABLE atlas_driver_offer_bpp.mg_eligibility ADD CONSTRAINT mg_eligibility_unique_idx_campaign_id_driver_id UNIQUE (campaign_id, driver_id);

CREATE INDEX CONCURRENTLY mg_eligibility_idx_driver_id_invited_at ON atlas_driver_offer_bpp.mg_eligibility USING btree (driver_id, invited_at);
