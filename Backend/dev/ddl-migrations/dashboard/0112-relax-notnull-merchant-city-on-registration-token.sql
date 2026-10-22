-- Expand-contract, phase 1 (rolling-deploy safe).
--
-- One token per person: the new code authorizes by URL + merchant_access and no
-- longer writes merchant_id / operating_city when minting a token. Those columns
-- are NOT NULL (0001 / 0027), so new inserts would violate the constraint. This
-- migration only RELAXES the NOT NULL so the new code can insert, WITHOUT dropping
-- the columns -- pre-refactor pods still SELECT them and existing rows keep their
-- values, so old and new pods coexist during the rollout.
--
-- The destructive DROP COLUMN is deliberately NOT here. It is a phase-2 migration
-- to be added and run ONLY AFTER the rollout is complete and no pre-refactor pods
-- remain (see 01NN-drop-merchant-city-from-registration-token.sql in a follow-up
-- release). Idempotent: DROP NOT NULL is a no-op if already nullable.
ALTER TABLE atlas_dashboard.registration_token
ALTER COLUMN merchant_id DROP NOT NULL;

ALTER TABLE atlas_dashboard.registration_token
ALTER COLUMN operating_city DROP NOT NULL;
--------------------------------------------------------- END ---------------------------------------------------------
