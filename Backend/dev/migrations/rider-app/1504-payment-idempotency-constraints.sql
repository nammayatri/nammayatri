-- Unique index on txn_uuid (partial: only non-null values) to prevent duplicate
-- transaction records from concurrent webhook deliveries.
-- The Redis lock is the primary guard, but this is a DB-level safety net.

-- Pre-check: find and back up duplicates before adding unique constraint.
-- Duplicates are moved to a backup table for manual review instead of being silently deleted.
DO $$
BEGIN
  IF EXISTS (
    SELECT txn_uuid, COUNT(*) FROM atlas_app.payment_transaction
    WHERE txn_uuid IS NOT NULL GROUP BY txn_uuid HAVING COUNT(*) > 1
  ) THEN
    RAISE NOTICE 'DUPLICATE txn_uuid VALUES FOUND - backing up duplicates before deduplication...';

    -- Create backup table to preserve duplicate records for audit
    CREATE TABLE IF NOT EXISTS atlas_app.payment_transaction_duplicate_backup AS
      SELECT * FROM atlas_app.payment_transaction WHERE false;

    -- Insert duplicate records (all except the most recent per txn_uuid) into backup
    INSERT INTO atlas_app.payment_transaction_duplicate_backup
    SELECT pt.* FROM atlas_app.payment_transaction pt
    WHERE pt.id NOT IN (
      SELECT DISTINCT ON (txn_uuid) id FROM atlas_app.payment_transaction
      WHERE txn_uuid IS NOT NULL ORDER BY txn_uuid, created_at DESC
    ) AND pt.txn_uuid IS NOT NULL
    AND pt.txn_uuid IN (
      SELECT txn_uuid FROM atlas_app.payment_transaction
      WHERE txn_uuid IS NOT NULL GROUP BY txn_uuid HAVING COUNT(*) > 1
    );

    RAISE NOTICE 'Backed up % duplicate rows to payment_transaction_duplicate_backup',
      (SELECT COUNT(*) FROM atlas_app.payment_transaction_duplicate_backup);

    -- Now remove the duplicates from the main table (keeping the most recent per txn_uuid)
    DELETE FROM atlas_app.payment_transaction
    WHERE id IN (SELECT id FROM atlas_app.payment_transaction_duplicate_backup);
  END IF;
END $$;

CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_payment_transaction_txn_uuid_unique
  ON atlas_app.payment_transaction (txn_uuid)
  WHERE txn_uuid IS NOT NULL;

-- Post-check: verify the index is VALID (CONCURRENTLY can leave it INVALID on failure)
DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM pg_class c JOIN pg_index i ON c.oid = i.indexrelid
    WHERE c.relname = 'idx_payment_transaction_txn_uuid_unique' AND NOT i.indisvalid
  ) THEN
    RAISE EXCEPTION 'INDEX idx_payment_transaction_txn_uuid_unique is INVALID - the concurrent build failed. Drop and retry.';
  END IF;
END $$;
