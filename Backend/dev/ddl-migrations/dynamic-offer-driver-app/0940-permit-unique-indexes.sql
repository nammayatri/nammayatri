-- One submitted or approved permit per conflict value (for PICNIC: vehicle, and driving
-- licence) per date, across all operating cities of a merchant. Partial indexes cannot
-- be declared in the NammaDSL spec.
CREATE UNIQUE INDEX IF NOT EXISTS permit_application_primary_conflict_idx
  ON atlas_driver_offer_bpp.permit_application (merchant_id, permit_type, permit_date, primary_conflict_hash)
  WHERE status IN ('SUBMITTED', 'APPROVED');

CREATE UNIQUE INDEX IF NOT EXISTS permit_application_secondary_conflict_idx
  ON atlas_driver_offer_bpp.permit_application (merchant_id, permit_type, permit_date, secondary_conflict_hash)
  WHERE status IN ('SUBMITTED', 'APPROVED');

CREATE UNIQUE INDEX IF NOT EXISTS permit_application_reference_no_idx
  ON atlas_driver_offer_bpp.permit_application (reference_no);

CREATE INDEX IF NOT EXISTS permit_application_qr_token_hash_idx
  ON atlas_driver_offer_bpp.permit_application (qr_token_hash);

CREATE UNIQUE INDEX IF NOT EXISTS permit_applicant_mobile_merchant_idx
  ON atlas_driver_offer_bpp.permit_applicant (mobile_number_hash, merchant_id);

-- The generator sizes every encrypted column as varchar(255). 'details' holds a whole
-- application as encrypted JSON, which is far longer.
ALTER TABLE atlas_driver_offer_bpp.permit_application ALTER COLUMN details_encrypted TYPE text;

-- The generator makes the hash of an optional encrypted field 'text', but the code
-- reads and writes it as bytes. With a text column every update re-encodes the stored
-- hash, after which the pass's QR code is no longer found.
ALTER TABLE atlas_driver_offer_bpp.permit_application ALTER COLUMN qr_token_hash TYPE bytea USING qr_token_hash::bytea;
