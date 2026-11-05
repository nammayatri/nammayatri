-- A ledger leg (reference + entity reference + from/to account, and the entry reversed for a reversal)
-- is posted at most once while not VOIDED and not reversed; the finance kernel returns the existing entry
-- on a repeat. A reversed entry (reversed_by set) is left out so the leg can be posted again. Rows before
-- the cutover predate the rule (multi-leg reversals, one row per gate fee item), so they are left out.
-- Set the cutover to a time after the rollout finished (no pod on the old code left: it posts legs this index
-- rejects and reverses without setting reversed_by), then run. If the build fails it leaves an INVALID index that
-- IF NOT EXISTS keeps: DROP INDEX CONCURRENTLY IF EXISTS finance_ledger_entry_leg_uniq; fix the duplicates; re-run.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS finance_ledger_entry_leg_uniq
  ON atlas_driver_offer_bpp.finance_ledger_entry USING btree (reference_type, reference_id, COALESCE(entity_reference_id, ''), from_account_id, to_account_id, COALESCE(reversal_of, ''))
  WHERE status <> 'VOIDED' AND reversed_by IS NULL AND created_at >= '2026-11-07 00:00:00+00';
