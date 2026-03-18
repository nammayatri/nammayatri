ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN IF NOT EXISTS hash_chain text;

CREATE INDEX IF NOT EXISTS idx_finance_audit_entry_merchant_created
  ON atlas_app.finance_audit_entry (merchant_id, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_finance_audit_entry_actor_type
  ON atlas_app.finance_audit_entry (merchant_id, actor_type, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_finance_audit_entry_entity_type_action
  ON atlas_app.finance_audit_entry (merchant_id, entity_type, action, created_at DESC);
