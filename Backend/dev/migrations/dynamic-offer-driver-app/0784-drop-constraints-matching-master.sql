------------------------------------------------------------------------------------------------
-- NOTE: This is to fix local schema to make it similar as master schema for config-sync import.
-- Do not run in master or prod
------------------------------------------------------------------------------------------------

-- Drop PK/UNIQUE constraints that master DB doesn't enforce.
-- Master data has duplicate values on these columns, so constraints can't be maintained.

ALTER TABLE atlas_driver_offer_bpp.bap_metadata DROP CONSTRAINT IF EXISTS bap_metadata_pkey CASCADE;
ALTER TABLE atlas_driver_offer_bpp.kiosk_location_translation DROP CONSTRAINT IF EXISTS kiosk_location_translation_pkey CASCADE;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay DROP CONSTRAINT IF EXISTS unique_on_category_name CASCADE;
ALTER TABLE atlas_driver_offer_bpp.registry_map_fallback DROP CONSTRAINT IF EXISTS registry_map_fallback_pkey CASCADE;
ALTER TABLE atlas_driver_offer_bpp.value_add_np DROP CONSTRAINT IF EXISTS value_add_np_pkey CASCADE;
