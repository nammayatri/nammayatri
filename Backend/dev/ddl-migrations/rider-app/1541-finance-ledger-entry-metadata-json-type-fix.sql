-- finance_ledger_entry.metadata is Maybe Value (JSON) in the finance-kernel Beam type, but the
-- generated migration (migrations-read-only/.../finance_ledger_entry.sql) declares the column as
-- `text`. This was dormant until the Stripe refund flow (mkRefundMetadata) became the first writer
-- and reader of metadata ({refundRequestId, refundRequestStatus}) for ledger dedup, at which point
-- every refund-ledger read throws BeamRowReadError ColumnTypeMismatch (ByteString vs text).
-- Align the column to json so it matches the Haskell type (cf. ddl type-fixes 0413, 0791).
-- Guarded so it is a no-op when the column is already json (re-run / already-patched DBs).
DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'atlas_app' AND table_name = 'finance_ledger_entry'
      AND column_name = 'metadata' AND data_type = 'text'
  ) THEN
    ALTER TABLE atlas_app.finance_ledger_entry
      ALTER COLUMN metadata TYPE json USING NULLIF(metadata, '')::json;
  END IF;
END $$;
