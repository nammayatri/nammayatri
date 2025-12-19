-- Drop the existing email constraint that only includes merchant_id and email
ALTER TABLE atlas_driver_offer_bpp.person DROP CONSTRAINT IF EXISTS unique_email;

-- Add new constraint that includes role to allow same email for different roles under same merchant
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT unique_email_role UNIQUE (merchant_id, email, role);

-- Drop the existing identifier constraint that only includes merchant_id and identifier
ALTER TABLE atlas_driver_offer_bpp.person DROP CONSTRAINT IF EXISTS unique_identifier;

-- Add new constraint that includes role to allow same identifier for different roles under same merchant
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT unique_identifier_role UNIQUE (merchant_id, identifier, role);