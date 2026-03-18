-- M1: Enforce immutability of audit entries at the database level.
-- Prevents UPDATE and DELETE on finance_audit_entry regardless of application bugs.
-- See LAW 2: Immutability of History (append-only).

CREATE OR REPLACE FUNCTION atlas_driver_offer_bpp.prevent_audit_mutation()
RETURNS trigger AS $$
BEGIN
  RAISE EXCEPTION 'AUDIT IMMUTABILITY VIOLATION: UPDATE and DELETE are forbidden on finance_audit_entry. Audit records are append-only.';
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS no_audit_mutation ON atlas_driver_offer_bpp.finance_audit_entry;

CREATE TRIGGER no_audit_mutation
  BEFORE UPDATE OR DELETE ON atlas_driver_offer_bpp.finance_audit_entry
  FOR EACH ROW
  EXECUTE FUNCTION atlas_driver_offer_bpp.prevent_audit_mutation();

-- M4: Remove updatedAt column from audit entries (immutable records should not have it).
ALTER TABLE atlas_driver_offer_bpp.finance_audit_entry DROP COLUMN IF EXISTS updated_at;

-- Add composite index for findByEntity query pattern (entity_type, entity_id, created_at)
CREATE INDEX IF NOT EXISTS idx_finance_audit_entry_entity_type_id_created
  ON atlas_driver_offer_bpp.finance_audit_entry (entity_type, entity_id, created_at DESC);
