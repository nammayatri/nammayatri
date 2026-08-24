ALTER TABLE atlas_driver_offer_bpp.white_list_org
  ALTER COLUMN supported_beckn_protocols TYPE text[]
  USING NULL::text[];
