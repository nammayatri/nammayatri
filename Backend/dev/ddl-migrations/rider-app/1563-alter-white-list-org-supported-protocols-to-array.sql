ALTER TABLE atlas_app.white_list_org
  ALTER COLUMN supported_beckn_protocols TYPE text[]
  USING NULL::text[];
