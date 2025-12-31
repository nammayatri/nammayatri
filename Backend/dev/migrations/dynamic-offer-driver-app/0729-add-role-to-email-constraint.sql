-- Note: Do not run below alter commands in production.
-- Drop the existing email constraint that only includes merchant_id and email
ALTER TABLE atlas_driver_offer_bpp.person DROP CONSTRAINT IF EXISTS unique_email;

-- Add new constraint that includes role to allow same email for different roles under same merchant
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT unique_email_role UNIQUE (merchant_id, email, role);

-- Drop the existing identifier constraint that only includes merchant_id and identifier
ALTER TABLE atlas_driver_offer_bpp.person DROP CONSTRAINT IF EXISTS unique_identifier;

-- Add new constraint that includes role to allow same identifier for different roles under same merchant
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT unique_identifier_role UNIQUE (merchant_id, identifier, role);






-- Create indexes concurrently
CREATE UNIQUE INDEX CONCURRENTLY idx_person_email_role ON atlas_driver_offer_bpp.person (merchant_id, email, role); --(These must be run outside a transaction block.)
CREATE UNIQUE INDEX CONCURRENTLY idx_person_identifier_role ON atlas_driver_offer_bpp.person (merchant_id, identifier, role); --(These must be run outside a transaction block.)

-- Drop old constraints FIRST
ALTER TABLE atlas_driver_offer_bpp.person DROP CONSTRAINT IF EXISTS unique_email;
ALTER TABLE atlas_driver_offer_bpp.person DROP CONSTRAINT IF EXISTS unique_identifier;

-- Then add new constraints using the indexes
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT unique_email_role UNIQUE USING INDEX idx_person_email_role;
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT unique_identifier_role UNIQUE USING INDEX idx_person_identifier_role;