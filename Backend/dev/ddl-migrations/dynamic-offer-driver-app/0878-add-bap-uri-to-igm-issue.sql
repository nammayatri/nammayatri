-- Mirrors rider-app 1565. IGMIssue's Beam type is shared between both platforms, so a
-- column added for the rider-app seller must exist here too or driver-app reads break.
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN bap_uri text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN beckn_transaction_id text;
