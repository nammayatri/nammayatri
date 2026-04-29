--- ONLY FOR LOCAL SYNC | Dont run in master or prod
ALTER TABLE atlas_driver_offer_bpp.merchant ADD COLUMN domain text;
ALTER TABLE atlas_driver_offer_bpp.merchant ADD COLUMN type text;
ALTER TABLE atlas_driver_offer_bpp.merchant ADD COLUMN api_key text;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN origin_restriction DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN destination_restriction DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN short_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN state DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN country DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant
  ALTER COLUMN short_id SET NOT NULL;
