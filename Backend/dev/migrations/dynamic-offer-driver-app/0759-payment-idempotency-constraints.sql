-- Unique index on txn_uuid (partial: only non-null values) to prevent duplicate
-- transaction records from concurrent webhook deliveries.
-- The Redis lock is the primary guard, but this is a DB-level safety net.

-- Pre-check: find duplicates before adding unique constraint
-- If this query returns rows, deduplicate before proceeding
DO $$
BEGIN
  IF EXISTS (
    SELECT txn_uuid, COUNT(*) FROM atlas_driver_offer_bpp.payment_transaction
    WHERE txn_uuid IS NOT NULL GROUP BY txn_uuid HAVING COUNT(*) > 1
  ) THEN
    RAISE NOTICE 'DUPLICATE txn_uuid VALUES FOUND - deduplicating...';
    -- Keep the most recent record for each txn_uuid
    DELETE FROM atlas_driver_offer_bpp.payment_transaction
    WHERE id NOT IN (
      SELECT DISTINCT ON (txn_uuid) id FROM atlas_driver_offer_bpp.payment_transaction
      WHERE txn_uuid IS NOT NULL ORDER BY txn_uuid, created_at DESC
    ) AND txn_uuid IS NOT NULL
    AND txn_uuid IN (
      SELECT txn_uuid FROM atlas_driver_offer_bpp.payment_transaction
      WHERE txn_uuid IS NOT NULL GROUP BY txn_uuid HAVING COUNT(*) > 1
    );
  END IF;
END $$;

CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_payment_transaction_txn_uuid_unique
  ON atlas_driver_offer_bpp.payment_transaction (txn_uuid)
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
