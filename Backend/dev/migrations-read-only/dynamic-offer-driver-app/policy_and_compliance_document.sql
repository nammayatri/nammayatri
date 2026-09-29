CREATE TABLE atlas_driver_offer_bpp.policy_and_compliance_document ();

ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD COLUMN enabled boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD COLUMN id character(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD COLUMN is_mandatory boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD COLUMN merchant_id character(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD COLUMN merchant_operating_city_id character(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD COLUMN metadata text ;
ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD COLUMN policy_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD COLUMN url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD COLUMN version text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.policy_and_compliance_document ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY policy_and_compliance_document_idx_created_at_enabled_merchant_id_policy_type ON atlas_driver_offer_bpp.policy_and_compliance_document USING btree (created_at, enabled, merchant_id, policy_type);