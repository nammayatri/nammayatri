

ALTER TABLE atlas_app.merchant ALTER COLUMN signing_public_key SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN signature_expiry SET NOT NULL;