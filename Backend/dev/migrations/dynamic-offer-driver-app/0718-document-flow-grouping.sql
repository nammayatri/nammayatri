ALTER TABLE atlas_driver_offer_bpp.document_verification_config
    ADD COLUMN IF NOT EXISTS document_flow_grouping text DEFAULT 'STANDARD';

