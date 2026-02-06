-- Add completedAt column to track when refund reaches terminal status
ALTER TABLE atlas_app.refunds
ADD COLUMN completed_at timestamp with time zone;

COMMENT ON COLUMN atlas_app.refunds.completed_at IS
  'Timestamp when refund reached terminal status (SUCCESS or FAILURE)';

-- Backfill existing terminal refunds with updated_at as completion time
UPDATE atlas_app.refunds
SET completed_at = updated_at
WHERE status IN ('REFUND_SUCCESS', 'REFUND_FAILURE')
  AND completed_at IS NULL;
